#include <generator>

#include <optional>
#include <utility>

#include <iostream>
#include <fstream>

#include <cctype>
#include <cassert>

#include <string_view>

#include <variant>
#include <list>
#include <unordered_map>
#include <vector>

#include <memory>

#include <algorithm>

#include <expected>

#include <format>
#include <sstream>

template <typename ...Ts>
struct overloaded : Ts... {
	using Ts::operator()...;

	overloaded(Ts &&...ts)
	: Ts{std::forward<Ts>(ts)}... { };
};

struct raw_symbol {
	std::string value;
};

// ---------------------------------------------------------------------
// Tokenization
// ---------------------------------------------------------------------

struct lparen { };
struct rparen { };

struct dot { };
struct quote { };

struct eof { };

using token = std::variant<
	raw_symbol,
	lparen,
	rparen,
	dot,
	quote,
	eof>;

inline constexpr std::string_view SPECIAL_CHARACTERS = "()'";

std::generator<token> tokenize(std::ifstream file) {
	auto valid_symbol_chr = [] (char c) {
		return !SPECIAL_CHARACTERS.contains(c) && !isspace(c);
	};

	while (!file.eof()) {
		char c = file.get();

		if (isspace(c)) {
			continue;
		} else if (c == ';') {
			while (!file.eof() && file.get() != '\n')
				;
		} else if (c == '(') {
			co_yield lparen{};
		} else if (c == ')') {
			co_yield rparen{};
		} else if (c == '\'') {
			co_yield quote{};
		} else {
			std::string v{};

			assert(valid_symbol_chr(c));

			while (valid_symbol_chr(c) && !file.eof()) {
				v.push_back(c);
				c = file.get();
			}

			// Return the last character since it's not a
			// part of the symbol
			if (!file.eof() && !valid_symbol_chr(c)) {
				file.unget();
			}

			// Special case: cons dot
			if (v == ".") {
				co_yield dot{};
			} else if (v.size() != 0) {
				co_yield raw_symbol{std::move(v)};
			}
		}
	}

	co_yield eof{};
}

std::ostream &operator<<(std::ostream &os, const token &tok) {
	std::visit(
		overloaded(
			[&] (const raw_symbol &sym) { os << "[sym " << sym.value << "]"; },
			[&] (lparen) { os << "("; },
			[&] (rparen) { os << ")"; },
			[&] (quote) { os << "'"; },
			[&] (dot) { os << "."; },
			[&] (eof) { os << "[eof]"; }
		), tok);
	return os;
}

// ---------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------

enum class error_kind {
	syntax_error,
	end_of_file,
	unrecognized_form,
	unbound_variable,
	illegal_argument,
};

struct error {
	error_kind kind;

	// TODO: maybe this ought to be a valuep or array thereof?
	// although token would also be useful here (especially if it
	// carried location information)
	std::string context = "";

	friend std::ostream &operator<<(std::ostream &os, const error &err) {
		switch (err.kind) {
			using enum error_kind;

			case syntax_error:
				os << "syntax error";
				break;
			case end_of_file:
				os << "end of file";
				break;
			case unrecognized_form:
				os << "unrecognized form: " << err.context;
				break;
			case unbound_variable:
				os << "unbound variable: " << err.context;
				break;
			case illegal_argument:
				os << "illegal_argument: " << err.context;
				break;

			default:
				os << "invalid error? " << (int)err.kind;
		}

		return os;
	}
};

template <typename T>
using result = std::expected<T, error>;

namespace {

template<typename T>
T value_or_void(result<T> &ex) {
	if constexpr (!std::is_same_v<T, void>)
		return std::move(ex.value());
}

[[noreturn, maybe_unused]] void abort_due_to_error(const error &err) {
	std::cerr << "aborting due to error: " << err << "\n";
	abort();
}

} // namespace anonymous

template <typename ...Ts>
std::unexpected<error> fail(Ts &&...ts) {
	return std::unexpected{error{std::forward<Ts>(ts)...}};
}

#define TRY(expr) \
	({ \
		auto TRY_result__ = (expr); \
		if (!TRY_result__) \
			return std::unexpected{TRY_result__.error()}; \
		value_or_void(TRY_result__); \
	})

#define MUST(expr) \
	({ \
		auto MUST_result__ = (expr); \
		if (!MUST_result__) \
			abort_due_to_error(MUST_result__.error()); \
		value_or_void(MUST_result__); \
	})

// ---------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------

struct value;
struct environment;

using valuep = std::shared_ptr<value>;

struct symbol {
	std::string value;
};

struct number {
	int value;
};

struct nil { };

struct cons {
	valuep car;
	valuep cdr;
};

struct function_formals {
	valuep formals;
};

struct lambda {
	function_formals formals;
	valuep body;

	std::shared_ptr<environment> captured_environment;
};

struct builtin {
	function_formals formals;

	std::string name;

	using fn_type = valuep (*)(std::vector<valuep> params, std::shared_ptr<environment> env);

	bool evaluate_params;
	fn_type tgt;
};

using value_variant = std::variant<symbol, number, nil, cons, lambda, builtin>;
struct value : value_variant {
	template <typename T>
	value(T &&t)
	: value_variant{std::forward<T>(t)} { }

	friend std::ostream &operator<<(std::ostream &os, const value &v) {
		std::visit(
			overloaded(
				[&] (const symbol &sym) { os << sym.value; },
				[&] (const number &num) { os << num.value; },
				[&] (nil) { os << "()"; },
				[&] (const cons &kons) {
					os << "(" << *kons.car;

					auto sub_cons = kons.cdr;

					while (sub_cons) {
						if (auto kons = std::get_if<cons>(sub_cons.get())) {
							os << " " << *kons->car;
							sub_cons = kons->cdr;
						} else if (std::get_if<nil>(sub_cons.get())) {
							os << ")";
							return;
						} else {
							// Improper list
							os << " . " << *sub_cons << ")";
							return;
						}
					}
				},
				[&] (const lambda &lmbd) {
					os << "[lambda " << &v << " " << *lmbd.formals.formals << " (env " << lmbd.captured_environment.get() << ")]";
				},
				[&] (const builtin &bltn) {
					os << "[builtin " << bltn.name << " " << *bltn.formals.formals << "]";
				}
			), v);
		return os;
	}
};


template <>
struct std::formatter<value, char> {
	template<class Ctx>
	constexpr Ctx::iterator parse(Ctx &ctx) {
		return ctx.begin();
	}

	template<class Ctx>
	Ctx::iterator format(value val, Ctx& ctx) const {
		std::ostringstream os;
		os << val;

		return std::ranges::copy(std::move(os).str(), ctx.out()).out;
	}
};

valuep make_symbol(std::string sym) {
	return std::make_shared<value>(symbol{std::move(sym)});
}

valuep make_number(int num) {
	return std::make_shared<value>(number{num});
}

valuep make_cons(valuep car, valuep cdr) {
	return std::make_shared<value>(cons{std::move(car), std::move(cdr)});
}

valuep make_lambda(function_formals formals, valuep body, std::shared_ptr<environment> captured_environment) {
	return std::make_shared<value>(lambda{std::move(formals), std::move(body), std::move(captured_environment)});
}

valuep make_functionlike_builtin(std::string name, builtin::fn_type tgt) {
	return std::make_shared<value>(builtin{function_formals{}, std::move(name), true, tgt});
}

valuep make_macrolike_builtin(std::string name, builtin::fn_type tgt) {
	return std::make_shared<value>(builtin{function_formals{}, std::move(name), false, tgt});
}

valuep make_nil() {
	return std::make_shared<value>(nil{});
}

// ---------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------

// TODO: end-of-file in the middle of an expression should be transformed into syntax_error
template <typename It>
struct parser {
	result<valuep> operator()(raw_symbol sym) {
		if ((sym.value[0] == '-' && sym.value.size() > 1 && std::ranges::all_of(sym.value.substr(1), ::isdigit))
				|| std::ranges::all_of(sym.value, ::isdigit))
			return std::make_shared<value>(number{std::stoi(sym.value)});

		return std::make_shared<value>(symbol{std::move(sym.value)});
	}

	result<valuep> operator()(quote) {
		return make_cons(
			make_symbol("quote"),
			make_cons(TRY(parse_expr()), make_nil()));
	}

	result<valuep> operator()(lparen) {
		auto token = *it;
		++it;

		// Is this closing a list?
		if (std::get_if<rparen>(&token))
			return make_nil();

		// Is this . x)? (a continuation of a cons or improper list)
		if (std::get_if<dot>(&token)) {
			auto cdr = TRY(parse_expr());

			auto tok_rparen = *it;
			++it;

			if (!std::get_if<rparen>(&tok_rparen))
				return fail(error_kind::syntax_error);
			return cdr;
		}

		auto car = TRY(std::visit(*this, token));

		// Recurse to parse the remainder of the list
		return make_cons(std::move(car), TRY((*this)(lparen{})));
	}

	// Invalid token at this point
	result<valuep> operator()(auto) {
		return fail(error_kind::syntax_error);
	}

	result<valuep> operator()(eof) {
		return fail(error_kind::end_of_file);
	}

	result<valuep> parse_expr() {
		auto token = *it;
		++it;
		return std::visit(*this, token);
	}

	It &it;
};

result<valuep> parse_expr(auto &it) {
	return parser{it}.parse_expr();
}

// ---------------------------------------------------------------------
// Evaluation
// ---------------------------------------------------------------------

struct environment : std::enable_shared_from_this<environment> {
	std::unordered_map<std::string, valuep> values;
	std::weak_ptr<environment> parent;

	result<valuep> lookup(std::string name) {
		auto in = shared_from_this();

		while (in) {
			auto it = in->values.find(name);
			if (it != in->values.end())
				return it->second;

			in = in->parent.lock();
		}

		return fail(error_kind::unbound_variable, name);
	}
};

template <typename F>
result<valuep> map(valuep list, F &&func) {
	if (std::get_if<nil>(list.get())) {
		return make_nil();
	} else if (auto kons = std::get_if<cons>(list.get())) {
		return make_cons(
			TRY(func(kons->car)), TRY(map(kons->cdr, std::forward<F>(func))));
	} else {
		return fail(error_kind::illegal_argument, std::format("in map: {} is not cons or nil", *list));
	}

}

result<valuep> eval(valuep expr, std::shared_ptr<environment> env) {
	if (std::get_if<number>(expr.get())) {
		return expr;
	} else if (auto sym = std::get_if<symbol>(expr.get())) {
		return env->lookup(sym->value);
	} else {
		//std::cout << "eval expr "<< *expr << "\n";
		auto kons = std::get_if<cons>(expr.get());
		assert(kons);

		// TODO: function-like builtins should be handled by the lambda case

		// Special cases: special forms
		if (auto sym = std::get_if<symbol>(kons->car.get())) {
			if (sym->value == "lambda" || sym->value == "λ") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto formals = kons->car;

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto body = kons->car;

				assert(std::get_if<nil>(kons->cdr.get()));

				return make_lambda(function_formals{formals}, body, env);
			} else if (sym->value == "if") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto condition = kons->car;

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto yes = kons->car;

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto no = kons->car;

				assert(std::get_if<nil>(kons->cdr.get()));

				auto conditionv = TRY(eval(condition, env));
				if (auto val = std::get_if<number>(conditionv.get()); !val || val->value) {
					return eval(yes, env);
				} else {
					return eval(no, env);
				}
			} else if (sym->value == "eq?") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto left = TRY(eval(kons->car, env));

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto right = TRY(eval(kons->car, env));

				if (auto left_num = std::get_if<number>(left.get()),
						right_num = std::get_if<number>(right.get());
						left_num && right_num && left_num->value == right_num->value) {
					return make_number(1);
				} else if (auto left_sym = std::get_if<symbol>(left.get()),
						right_sym = std::get_if<symbol>(right.get());
						left_sym && right_sym && left_sym->value == right_sym->value) {
					return make_number(1);
				} else {
					return make_number(0);
				}
			} else if (sym->value == "+") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto left = TRY(eval(kons->car, env));

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto right = TRY(eval(kons->car, env));

				if (auto left_num = std::get_if<number>(left.get()),
						right_num = std::get_if<number>(right.get());
						left_num && right_num) {
					return make_number(left_num->value + right_num->value);
				} else {
					assert(!"addition of not-numbers");
				}
			} else if (sym->value == "*") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto left = TRY(eval(kons->car, env));

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto right = TRY(eval(kons->car, env));

				if (auto left_num = std::get_if<number>(left.get()),
						right_num = std::get_if<number>(right.get());
						left_num && right_num) {
					return make_number(left_num->value * right_num->value);
				} else {
					assert(!"multiplication of not-numbers");
				}
			} else if (sym->value == "define") {
				// define has two forms: (define name value) and (define (name formals...) body)

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto first = kons->car;

				// This is the former form
				if (auto sym = std::get_if<symbol>(first.get())) {
					kons = std::get_if<cons>(kons->cdr.get());
					assert(kons);
					auto value = TRY(eval(kons->car, env));

					env->values[sym->value] = value;

					assert(std::get_if<nil>(kons->cdr.get()));

					return value;
				} else {
					auto first_kons = std::get_if<cons>(first.get());
					assert(first_kons);
					auto name = std::get_if<symbol>(first_kons->car.get());
					assert(name);
					auto formals = first_kons->cdr;

					kons = std::get_if<cons>(kons->cdr.get());
					assert(kons);
					auto body = kons->car;

					auto value = make_lambda(function_formals{formals}, body, env);
					env->values[name->value] = value;

					assert(std::get_if<nil>(kons->cdr.get()));
					return value;
				}
			} else if (sym->value == "car") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto arg = TRY(eval(kons->car, env));

				auto kons = std::get_if<cons>(arg.get());
				assert(kons);
				return kons->car;
			} else if (sym->value == "cdr") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto arg = TRY(eval(kons->car, env));

				auto kons = std::get_if<cons>(arg.get());
				assert(kons);
				return kons->cdr;
			} else if (sym->value == "cons") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto car = TRY(eval(kons->car, env));

				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto cdr = TRY(eval(kons->car, env));

				return make_cons(car, cdr);
			} else if (sym->value == "nil?") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);
				auto arg = TRY(eval(kons->car, env));

				return make_number(!!std::get_if<nil>(arg.get()));
			} else if (sym->value == "quote") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);

				return kons->car;
			} else if (sym->value == "eval") {
				kons = std::get_if<cons>(kons->cdr.get());
				assert(kons);

				auto expr = TRY(eval(kons->car, env));
				return eval(expr, env); // TODO: env?

			}
		}

		// Not a special form
		auto fn = TRY(eval(kons->car, env));

		if (auto bltn = std::get_if<builtin>(fn.get())) {
			auto params_cons = bltn->evaluate_params
				? TRY(map(kons->cdr, [&] (valuep expr) { return eval(expr, env); }))
				: kons->cdr;

			std::vector<valuep> params;

			cons *kons = std::get_if<cons>(params_cons.get());
			while (kons) {
				params.push_back(kons->car);
				kons = std::get_if<cons>(kons->cdr.get());
			}

			return bltn->tgt(params, env);
		} else if (auto lmbd = std::get_if<lambda>(fn.get())) {
			auto sub_env = std::make_shared<environment>();
			sub_env->parent = lmbd->captured_environment;

			if (auto formals_sym = std::get_if<symbol>(lmbd->formals.formals.get())) {
				sub_env->values[formals_sym->value] = TRY(
					map(kons->cdr, [&] (valuep expr) { return eval(expr, env); }));

				return eval(lmbd->body, sub_env);
			}

			auto formals_cons = std::get_if<cons>(lmbd->formals.formals.get());
			assert(formals_cons);
			auto params_cons = std::get_if<cons>(kons->cdr.get());
			assert(params_cons);

			while (formals_cons && params_cons) {
				auto formal = std::get_if<symbol>(formals_cons->car.get());
				assert(formal);
				auto value = params_cons->car;

				sub_env->values[formal->value] = TRY(eval(value, env));

				// Special case: formals are an improper list
				if (auto last_formal = std::get_if<symbol>(formals_cons->cdr.get())) {
					sub_env->values[last_formal->value] = TRY(
						map(params_cons->cdr, [&] (valuep expr) { return eval(expr, env); }));
					break;
				}

				// If this is the end of formals, make sure the argument list ends as well
				if (std::get_if<nil>(formals_cons->cdr.get()))
					assert(std::get_if<nil>(params_cons->cdr.get()));

				// If this is the end of params, make sure the formals end as well
				if (std::get_if<nil>(params_cons->cdr.get()))
					assert(std::get_if<nil>(formals_cons->cdr.get()));

				assert(std::get_if<cons>(formals_cons->cdr.get()) || std::get_if<nil>(formals_cons->cdr.get()));
				formals_cons = std::get_if<cons>(formals_cons->cdr.get());

				assert(std::get_if<cons>(params_cons->cdr.get()) || std::get_if<nil>(params_cons->cdr.get()));
				params_cons = std::get_if<cons>(params_cons->cdr.get());
			}

			return eval(lmbd->body, sub_env);
		} else {
			return fn;
		}
	}
}

// ---------------------------------------------------------------------
// Misc
// ---------------------------------------------------------------------

int main(int argc, char **argv) {
	if(argc != 2) {
		std::cerr << argv[0] << "requires an argument.\n";
		return 1;
	}

	std::ifstream file{argv[1]};
	if (!file) {
		std::cerr << "Failed to open file\n";
		return 2;
	}

	auto env = std::make_shared<environment>();

	auto tokens = tokenize(std::move(file));

	auto it = tokens.begin();

	while (true) {
		auto maybe_expr = parse_expr(it);
		if (!maybe_expr && maybe_expr.error().kind == error_kind::end_of_file)
			break;

		auto expr = MUST(maybe_expr);

		std::cout << "=> " << *expr << "\n";

		std::cout << "<= " << *MUST(eval(expr, env)) << "\n";
	}
}
