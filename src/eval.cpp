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


#include "eval.hpp"

#include <cassert>
#include <format>


template <typename F>
result<valuep> map(valuep list, F &&func) {
	if (value_cast<nil>(list)) {
		return make_nil();
	} else if (auto kons = value_cast<cons>(list)) {
		return make_cons(
			TRY(func(kons->car)), TRY(map(kons->cdr, std::forward<F>(func))));
	} else {
		return fail(error_kind::illegal_argument, std::format("in map: {} is not cons or nil", *list));
	}
}

result<valuep> eval(valuep expr, std::shared_ptr<environment> env) {
	if (value_cast<number>(expr)) {
		return expr;
	} else if (auto sym = value_cast<symbol>(expr)) {
		return env->lookup(sym->val);
	} else {
		auto kons = value_cast<cons>(expr);
		if (!kons)
			return fail(error_kind::unrecognized_form,
					std::format("unrecognized form {}", *expr));

		auto fn = TRY(eval(kons->car, env));

		if (auto bltn = value_cast<builtin>(fn)) {
			auto params_cons = bltn->evaluate_params
				? TRY(map(kons->cdr, [&] (valuep expr) { return eval(expr, env); }))
				: kons->cdr;

			std::vector<valuep> params;

			cons *kons = value_cast<cons>(params_cons);
			while (kons) {
				params.push_back(kons->car);
				kons = value_cast<cons>(kons->cdr);
			}

			return bltn->tgt(params, env);
		} else if (auto lmbd = value_cast<lambda>(fn)) {
			auto sub_env = std::make_shared<environment>();
			sub_env->parent = lmbd->captured_environment;

			TRY(lmbd->formals.map_params(
					kons->cdr,
					[&] (std::string name, bool rest, valuep expr) -> result<void> {
						sub_env->values[name] =
							lmbd->is_macro
							? expr : rest
							? TRY(map(expr, [&] (valuep expr) {
								return eval(expr, env);
							})) : TRY(eval(expr, env));
						return {};
					}));

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


namespace {

result<valuep> quasi_unquote(valuep expr, std::shared_ptr<environment> env);

std::tuple<valuep, bool> unpack_unquote(cons *kons) {
	if (auto sym = value_cast<symbol>(kons->car);
			sym && (sym->val == "unquote"
					|| sym->val == "unquote-splicing")) {
		if (auto unquoted_cons = value_cast<cons>(kons->cdr);
				unquoted_cons && value_cast<nil>(unquoted_cons->cdr)) {
			return std::make_tuple(unquoted_cons->car,
					sym->val == "unquote-splicing");
		}
	}

	return std::make_tuple(nullptr, false);
}

result<std::tuple<valuep, bool>>
quasi_eval_unquote(valuep expr, std::shared_ptr<environment> env) {
	if (auto kons = value_cast<cons>(expr)) {
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
	if (auto kons = value_cast<cons>(expr)) {
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
		auto out_cur = value_cast<cons>(out);

		while (in_cur) {
			auto [expr, splice] = TRY(quasi_eval_unquote(in_cur->car, env));

			if (splice) {
				auto new_cons = value_cast<cons>(expr);
				// !new_cons happens for e.g. `(,@1) => 1
				if (!new_cons) {
					auto out_cons = value_cast<cons>(out);
					assert(out_cons);

					// Make sure out was empty
					if (!value_cast<nil>(out_cons->car)
							|| !value_cast<nil>(out_cons->cdr))
						return fail(error_kind::illegal_argument,
								std::format("unquote-splicing of a non-cons replaces the outer list, but it's not empty: {}",
										*out));

					// Make sure in_cur->cdr is nil (since we're discarding the rest of the input)
					if (!value_cast<nil>(in_cur->cdr))
						return fail(error_kind::illegal_argument,
								std::format("unquote-splicing of a non-cons replaces the outer list, but the input list has trailing elements: {}",
										*in_cur->cdr));


					// And just replace the whole list with the unquoted expr
					return expr;
				}

				*out_cur = *new_cons;

				// Advance out_cur s.t. out_cur->cdr is not a cons
				while (value_cast<cons>(out_cur->cdr))
					out_cur = value_cast<cons>(out_cur->cdr);
			} else {
				out_cur->car = expr;
			}

			auto next_in_cons = value_cast<cons>(in_cur->cdr);

			if (!next_in_cons) { // improper list
				if (!value_cast<nil>(in_cur->cdr)) {
					out_cur->cdr = TRY(quasi_unquote(in_cur->car, env));
				}
			} else {
				// Make sure the output cdr is nil (if we've unspliced an
				// improper list, it could be something else)

				if (!value_cast<nil>(out_cur->cdr))
					return fail(error_kind::illegal_argument,
							std::format("unquote-splicing produced an improper list, but we're not done with quasiquote! (cdr output) => {}, (cdr input) => {}",
									*out_cur->cdr,
									*in_cur->cdr));

				// Add a new empty cons in the output and advance to it
				out_cur->cdr = make_cons(make_nil(), make_nil());
				out_cur = value_cast<cons>(out_cur->cdr);
			}

			in_cur = next_in_cons;
		}

		return out;
	}

	// Nothing special, move along
	return expr;
}

} // namespace anonymous


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

			auto kons = value_cast<cons>(params[0]);
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

			auto kons = value_cast<cons>(params[0]);
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

			return make_number(!!value_cast<nil>(params[0]));
		});

	functionlike(
		"eq?",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			if (params.size() != 2)
				return fail(error_kind::unrecognized_form, "eq? takes 2 parameter");

			if (auto left_num = value_cast<number>(params[0]),
					right_num = value_cast<number>(params[1]);
					left_num && right_num && left_num->val == right_num->val) {
				return make_number(1);
			} else if (auto left_sym = value_cast<symbol>(params[0]),
					right_sym = value_cast<symbol>(params[1]);
					left_sym && right_sym && left_sym->val == right_sym->val) {
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
				auto num = value_cast<number>(param);
				if (!num)
					return fail(error_kind::illegal_argument,
							std::format("+ expects a number, not {}", *param));

				result += num->val;
			}

			return make_number(result);
		});

	functionlike(
		"*",
		[] (std::vector<valuep> params, std::shared_ptr<environment>) -> result<valuep> {
			int result = 1;

			for (auto param : params) {
				auto num = value_cast<number>(param);
				if (!num)
					return fail(error_kind::illegal_argument,
							std::format("* expects a number, not {}", *param));

				result *= num->val;
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

			if (auto val = value_cast<number>(condition); !val || val->val) {
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

			return make_lambda(TRY(function_formals::parse(params[0])), params[1], env);
		});
	root_env->values["λ"] = root_env->values["lambda"];

	macrolike(
		"define",
		[] (std::vector<valuep> params, std::shared_ptr<environment> env) -> result<valuep> {
			if (params.size() != 2)
				return fail(error_kind::unrecognized_form, "define takes 2 parameters");

			if (auto name = value_cast<symbol>(params[0])) {
				// (define name value)

				auto value = TRY(eval(params[1], env));
				env->values[name->val] = value;
				return value;
			} else if (auto name_and_formals = value_cast<cons>(params[0])) {
				// (define (name formals...) body)

				auto name = value_cast<symbol>(name_and_formals->car);
				auto formals = name_and_formals->cdr;
				if (!name)
					return fail(error_kind::illegal_argument,
							std::format("define expects a symbol for name, not {}", *name_and_formals->car));

				auto value = make_lambda(TRY(function_formals::parse(formals)), params[1], env);
				env->values[name->val] = value;
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

			if (auto name_and_formals = value_cast<cons>(params[0])) {
				// (define-macro (name formals...) body)

				auto name = value_cast<symbol>(name_and_formals->car);
				auto formals = name_and_formals->cdr;
				if (!name)
					return fail(error_kind::illegal_argument,
							std::format("define-macro expects a symbol for name, not {}", *name_and_formals->car));

				auto value = make_macro(TRY(function_formals::parse(formals)), params[1], env);
				env->values[name->val] = value;
				return value;
			}
			return fail(error_kind::illegal_argument,
					std::format("define-macro expects a cons for name and formals, not {}", *params[0]));
		});

	return root_env;
}
