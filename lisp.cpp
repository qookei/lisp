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

#include "result.hpp"
#include "parse.hpp"
#include "token.hpp"
#include "value.hpp"
#include "util.hpp"


// ---------------------------------------------------------------------
// Evaluation
// ---------------------------------------------------------------------

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
			if (!formals_cons && !std::get_if<nil>(lmbd->formals.formals.get()))
				return fail(error_kind::unrecognized_form,
						std::format("lambda formals should be a list or symbol, not {}",
								*lmbd->formals.formals));

			auto params_cons = std::get_if<cons>(kons->cdr.get());
			if (!params_cons && !std::get_if<nil>(kons->cdr.get()))
				return fail(error_kind::unrecognized_form,
						std::format("lambda parameters should be a list, not {}",
								*kons->cdr));

			if (!params_cons && formals_cons)
				return fail(error_kind::illegal_argument,
						std::format("lambda takes parameters but none were given"));

			if (params_cons && !formals_cons)
				return fail(error_kind::illegal_argument,
						std::format("lambda given parameters but takes none"));

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

	auto tokens = tokenize(file);

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
