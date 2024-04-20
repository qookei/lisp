/*
  Lisp-like language interpreter.
  Copyright (C) 2024  qookie

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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
struct quasiquote { };
struct unquote { };
struct unquote_splicing { };

struct eof { };

using token = std::variant<
	raw_symbol,
	lparen,
	rparen,
	dot,
	quote,
	quasiquote,
	unquote,
	unquote_splicing,
	eof>;

// TODO: quotes can appear inside of symbols
inline constexpr std::string_view SPECIAL_CHARACTERS = "()'`,";

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
		} else if (c == '`') {
			co_yield quasiquote{};
		} else if (c == ',') {
			if (file.peek() == '@') {
				file.get();
				co_yield unquote_splicing{};
			} else {
				co_yield unquote{};
			}
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
			[&] (quasiquote) { os << "`"; },
			[&] (unquote) { os << ","; },
			[&] (unquote_splicing) { os << ",@"; },
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
				os << "illegal argument: " << err.context;
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

	bool is_macro;

	std::shared_ptr<environment> captured_environment;
};

struct builtin {
	function_formals formals;

	std::string name;

	using fn_type = result<valuep> (*)(std::vector<valuep> params, std::shared_ptr<environment> env);

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
					os << (lmbd.is_macro ? "[macro " : "[lambda ")
						<< &v << " " << *lmbd.formals.formals << " (env " << lmbd.captured_environment.get() << ")]";
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
	return std::make_shared<value>(lambda{std::move(formals), std::move(body), false, std::move(captured_environment)});
}

valuep make_macro(function_formals formals, valuep body, std::shared_ptr<environment> captured_environment) {
	return std::make_shared<value>(lambda{std::move(formals), std::move(body), true, std::move(captured_environment)});
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

	result<valuep> operator()(quasiquote) {
		return make_cons(
			make_symbol("quasiquote"),
			make_cons(TRY(parse_expr()), make_nil()));
	}

	result<valuep> operator()(unquote) {
		return make_cons(
			make_symbol("unquote"),
			make_cons(TRY(parse_expr()), make_nil()));
	}

	result<valuep> operator()(unquote_splicing) {
		return make_cons(
			make_symbol("unquote-splicing"),
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
		auto kons = std::get_if<cons>(expr.get());
		if (!kons)
			return fail(error_kind::unrecognized_form,
					std::format("unrecognized form {}", *expr));

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
				if (lmbd->is_macro) {
					sub_env->values[formals_sym->value] = kons->cdr;
					auto out_expr = TRY(eval(lmbd->body, sub_env));
					return eval(out_expr, env);
				}

				sub_env->values[formals_sym->value] = TRY(
					map(kons->cdr, [&] (valuep expr) { return eval(expr, env); }));

				return eval(lmbd->body, sub_env);
			}

			auto formals_cons = std::get_if<cons>(lmbd->formals.formals.get());
			if (!formals_cons)
				return fail(error_kind::unrecognized_form,
						std::format("lambda formals should be a list or symbol, not {}",
								*lmbd->formals.formals));

			auto params_cons = std::get_if<cons>(kons->cdr.get());
			if (!params_cons)
				return fail(error_kind::unrecognized_form,
						std::format("lambda parameters should be a list, not {}",
								*kons->cdr));

			while (formals_cons && params_cons) {
				auto formal = std::get_if<symbol>(formals_cons->car.get());
				if (!formal)
					return fail(error_kind::unrecognized_form,
						std::format("lambda formal should be a symbol, not {}",
								*formals_cons->car));

				auto value = params_cons->car;

				sub_env->values[formal->value] = lmbd->is_macro ? value : TRY(eval(value, env));

				// Special case: formals are an improper list
				if (auto last_formal = std::get_if<symbol>(formals_cons->cdr.get())) {
					if (lmbd->is_macro) {
						sub_env->values[last_formal->value] = params_cons->cdr;
						break;
					}

					sub_env->values[last_formal->value] = TRY(
						map(params_cons->cdr, [&] (valuep expr) { return eval(expr, env); }));
					break;
				}

				// If this is the end of formals, make sure the argument list ends as well
				if (std::get_if<nil>(formals_cons->cdr.get())
						&& !std::get_if<nil>(params_cons->cdr.get()))
					return fail(error_kind::unrecognized_form,
						std::format("too many parameters, left over parameters are {}",
								*params_cons->cdr));

				// If this is the end of params, make sure the formals end as well
				if (std::get_if<nil>(params_cons->cdr.get())
						&& !std::get_if<nil>(formals_cons->cdr.get()))
					return fail(error_kind::unrecognized_form,
						std::format("not enough parameters, missing parameters for {}",
								*formals_cons->cdr));

				if (!std::get_if<cons>(formals_cons->cdr.get()) &&
						!std::get_if<nil>(formals_cons->cdr.get()))
					return fail(error_kind::unrecognized_form,
						std::format("lambda formals should be a list, not {}",
								*formals_cons->cdr));


				if (!std::get_if<cons>(params_cons->cdr.get()) &&
						!std::get_if<nil>(params_cons->cdr.get()))
					return fail(error_kind::unrecognized_form,
						std::format("lambda parameters should be a list, not {}",
								*params_cons->cdr));

				formals_cons = std::get_if<cons>(formals_cons->cdr.get());
				params_cons = std::get_if<cons>(params_cons->cdr.get());
			}

			if (lmbd->is_macro) {
				auto out_expr = TRY(eval(lmbd->body, sub_env));
				return eval(out_expr, env);
			}

			return eval(lmbd->body, sub_env);
		} else {
			return fn;
		}
	}
}

result<valuep> quasi_unquote(valuep expr, std::shared_ptr<environment> env);

std::tuple<valuep, bool> unpack_unquote(cons *kons) {
	if (auto sym = std::get_if<symbol>(kons->car.get());
			sym && (sym->value == "unquote"
					|| sym->value == "unquote-splicing")) {
		if (auto unquoted_cons = std::get_if<cons>(kons->cdr.get());
				unquoted_cons && std::get_if<nil>(unquoted_cons->cdr.get())) {
			return std::make_tuple(unquoted_cons->car,
					sym->value == "unquote-splicing");
		}
	}

	return std::make_tuple(nullptr, false);
}

result<std::tuple<valuep, bool>>
quasi_eval_unquote(valuep expr, std::shared_ptr<environment> env) {
	if (auto kons = std::get_if<cons>(expr.get())) {
		if (auto [unquoted, splice] = unpack_unquote(kons); unquoted) {
			return std::make_tuple(
				TRY(eval(unquoted, env)),
				splice);
		}

		return std::make_tuple(TRY(quasi_unquote(expr, env)), false);
	}

	return std::make_tuple(expr, false);
}

result<valuep> quasi_unquote(valuep expr, std::shared_ptr<environment> env) {
	if (auto kons = std::get_if<cons>(expr.get())) {
		if (auto [unquoted, splice] = unpack_unquote(kons); unquoted) {
			if (!splice) {
				return eval(unquoted, env);
			} else {
				// Nothing to splice into at this point, return as-is.
				return expr;
			}
		}

		auto out = make_cons(make_nil(), make_nil());

		auto in_cur = kons;
		auto out_cur = std::get_if<cons>(out.get());

		while (in_cur) {
			auto [expr, splice] = TRY(quasi_eval_unquote(in_cur->car, env));

			if (splice) {
				auto new_cons = std::get_if<cons>(expr.get());
				// !new_cons happens for e.g. `(,@1) => 1
				if (!new_cons) {
					auto out_cons = std::get_if<cons>(out.get());
					assert(out_cons);

					// Make sure out was empty
					if (!std::get_if<nil>(out_cons->car.get())
							|| !std::get_if<nil>(out_cons->cdr.get()))
						return fail(error_kind::illegal_argument,
								std::format("unquote-splicing of a non-cons replaces the outer list, but it's not empty: {}",
										*out));

					// Make sure in_cur->cdr is nil (since we're discarding the rest of the input)
					if (!std::get_if<nil>(in_cur->cdr.get()))
						return fail(error_kind::illegal_argument,
								std::format("unquote-splicing of a non-cons replaces the outer list, but the input list has trailing elements: {}",
										*in_cur->cdr));


					// And just replace the whole list with the unquoted expr
					return expr;
				}

				*out_cur = *new_cons;

				// Advance out_cur s.t. out_cur->cdr is not a cons
				while (std::get_if<cons>(out_cur->cdr.get()))
					out_cur = std::get_if<cons>(out_cur->cdr.get());
			} else {
				out_cur->car = expr;
			}

			auto next_in_cons = std::get_if<cons>(in_cur->cdr.get());

			if (!next_in_cons) { // improper list
				if (!std::get_if<nil>(in_cur->cdr.get())) {
					out_cur->cdr = TRY(quasi_unquote(in_cur->car, env));
				}
			} else {
				// Make sure the output cdr is nil (if we've unspliced an
				// improper list, it could be something else)

				if (!std::get_if<nil>(out_cur->cdr.get()))
					return fail(error_kind::illegal_argument,
							std::format("unquote-splicing produced an improper list, but we're not done with quasiquote! (cdr output) => {}, (cdr input) => {}",
									*out_cur->cdr,
									*in_cur->cdr));

				// Add a new empty cons in the output and advance to it
				out_cur->cdr = make_cons(make_nil(), make_nil());
				out_cur = std::get_if<cons>(out_cur->cdr.get());
			}

			in_cur = next_in_cons;
		}

		return out;
	}

	// Nothing special, move along
	return expr;
}

std::shared_ptr<environment> prepare_root_environment() {
	auto root_env = std::make_shared<environment>();

	auto functionlike = [&] (std::string name, builtin::fn_type tgt) {
		root_env->values[name] = make_functionlike_builtin(name, tgt);
	};

	auto macrolike = [&] (std::string name, builtin::fn_type tgt) {
		root_env->values[name] = make_macrolike_builtin(name, tgt);
	};

	functionlike(
		"cons",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			if (params.size() != 2)
				return fail(error_kind::unrecognized_form, "cons takes 2 parameters");

			return make_cons(params[0], params[1]);
		});

	functionlike(
		"car",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			if (params.size() != 1)
				return fail(error_kind::unrecognized_form, "car takes 1 parameter");

			auto kons = std::get_if<cons>(params[0].get());
			if (!kons)
				return fail(error_kind::illegal_argument,
						std::format("car expects a cons, not {}", *params[0]));

			return kons->car;
		});

	functionlike(
		"cdr",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			if (params.size() != 1)
				return fail(error_kind::unrecognized_form, "cdr takes 1 parameter");

			auto kons = std::get_if<cons>(params[0].get());
			if (!kons)
				return fail(error_kind::illegal_argument,
						std::format("cdr expects a cons, not {}", *params[0]));

			return kons->cdr;
		});

	functionlike(
		"nil?",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			if (params.size() != 1)
				return fail(error_kind::unrecognized_form, "nil? takes 1 parameter");

			return make_number(!!std::get_if<nil>(params[0].get()));
		});

	functionlike(
		"eq?",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			if (params.size() != 2)
				return fail(error_kind::unrecognized_form, "eq? takes 2 parameter");

			if (auto left_num = std::get_if<number>(params[0].get()),
					right_num = std::get_if<number>(params[1].get());
					left_num && right_num && left_num->value == right_num->value) {
				return make_number(1);
			} else if (auto left_sym = std::get_if<symbol>(params[0].get()),
					right_sym = std::get_if<symbol>(params[1].get());
					left_sym && right_sym && left_sym->value == right_sym->value) {
				return make_number(1);
			} else {
				return make_number(0);
			}
		});

	functionlike(
		"+",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			int result = 0;

			for (auto param : params) {
				auto num = std::get_if<number>(param.get());
				if (!num)
					return fail(error_kind::illegal_argument,
							std::format("+ expects a number, not {}", *param));

				result += num->value;
			}

			return make_number(result);
		});

	functionlike(
		"*",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			int result = 1;

			for (auto param : params) {
				auto num = std::get_if<number>(param.get());
				if (!num)
					return fail(error_kind::illegal_argument,
							std::format("* expects a number, not {}", *param));

				result *= num->value;
			}

			return make_number(result);
		});

	functionlike(
		"eval",
		[] (std::vector<valuep> params, std::shared_ptr<environment> env) -> result<valuep> {
			if (params.size() != 1)
				return fail(error_kind::unrecognized_form, "eval takes 1 parameter");

			return eval(params[0], env); // TODO: env argument?
		});

	macrolike(
		"quote",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			if (params.size() != 1)
				return fail(error_kind::unrecognized_form, "quote takes 1 parameter");

			return params[0];
		});

	macrolike(
		"quasiquote",
		[] (std::vector<valuep> params, std::shared_ptr<environment> env) -> result<valuep> {
			if (params.size() != 1)
				return fail(error_kind::unrecognized_form, "quasiquote takes 1 parameter");

			return quasi_unquote(params[0], env);
		});

	macrolike(
		"if",
		[] (std::vector<valuep> params, std::shared_ptr<environment> env) -> result<valuep> {
			if (params.size() != 3)
				return fail(error_kind::unrecognized_form, "if takes 3 parameters");

			auto condition = TRY(eval(params[0], env));

			if (auto val = std::get_if<number>(condition.get()); !val || val->value) {
				return eval(params[1], env);
			} else {
				return eval(params[2], env);
			}
		});

	macrolike(
		"lambda",
		[] (std::vector<valuep> params, std::shared_ptr<environment> env) -> result<valuep> {
			if (params.size() != 2)
				return fail(error_kind::unrecognized_form, "lambda takes 2 parameters");

			return make_lambda(function_formals{params[0]}, params[1], env);
		});
	root_env->values["λ"] = root_env->values["lambda"];

	macrolike(
		"define",
		[] (std::vector<valuep> params, std::shared_ptr<environment> env) -> result<valuep> {
			if (params.size() != 2)
				return fail(error_kind::unrecognized_form, "define takes 2 parameters");

			if (auto name = std::get_if<symbol>(params[0].get())) {
				// (define name value)

				auto value = TRY(eval(params[1], env));
				env->values[name->value] = value;
				return value;
			} else if (auto name_and_formals = std::get_if<cons>(params[0].get())) {
				// (define (name formals...) body)

				auto name = std::get_if<symbol>(name_and_formals->car.get());
				auto formals = name_and_formals->cdr;
				if (!name)
					return fail(error_kind::illegal_argument,
							std::format("define expects a symbol for name, not {}", *name_and_formals->car));

				auto value = make_lambda(function_formals{formals}, params[1], env);
				env->values[name->value] = value;
				return value;
			}
			return fail(error_kind::illegal_argument,
					std::format("define expects either a cons for name and formals, or a symbol for name, not {}", *params[0]));
		});

	macrolike(
		"define-macro",
		[] (std::vector<valuep> params, std::shared_ptr<environment> env) -> result<valuep> {
			if (params.size() != 2)
				return fail(error_kind::unrecognized_form, "define-macro takes 2 parameters");

			if (auto name_and_formals = std::get_if<cons>(params[0].get())) {
				// (define-macro (name formals...) body)

				auto name = std::get_if<symbol>(name_and_formals->car.get());
				auto formals = name_and_formals->cdr;
				if (!name)
					return fail(error_kind::illegal_argument,
							std::format("define-macro expects a symbol for name, not {}", *name_and_formals->car));

				auto value = make_macro(function_formals{formals}, params[1], env);
				env->values[name->value] = value;
				return value;
			}
			return fail(error_kind::illegal_argument,
					std::format("define-macro expects a cons for name and formals, not {}", *params[0]));
		});

	return root_env;
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

	auto root_env = prepare_root_environment();
	auto env = std::make_shared<environment>();
	env->parent = root_env;

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
