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
#include <sstream>
#include <format>


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

		if (auto cb = value_cast<callable>(fn)) {
			return cb->apply(kons->cdr, env);
		} else {
			return fail(error_kind::unrecognized_form, std::format(
						"{} is not invocable", *fn));
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


namespace builtins {

result<valuep> cons_(valuep car, valuep cdr) {
	return make_cons(car, cdr);
}

result<valuep> car(cons *kons) {
	return kons->car;
}

result<valuep> cdr(cons *kons) {
	return kons->cdr;
}

result<valuep> nil_p(valuep expr) {
	return make_number(expr->type() == value_type::nil);
}

bool equal_p_impl(valuep left, valuep right) {
	if (left == right)
		return true;

	if (left->type() != right->type())
		return false;

	switch (left->type()) {
		case value_type::nil: {
			return true;
		}
		case value_type::symbol: {
			auto left_sym = value_cast<symbol>(left);
			assert(left_sym);
			auto right_sym = value_cast<symbol>(right);
			assert(right_sym);

			return left_sym->val == right_sym->val;
		}
		case value_type::number: {
			auto left_num = value_cast<number>(left);
			assert(left_num);
			auto right_num = value_cast<number>(right);
			assert(right_num);

			return left_num->val == right_num->val;
		}
		case value_type::cons: {
			auto left_cons = value_cast<cons>(left);
			assert(left_cons);
			auto right_cons = value_cast<cons>(right);
			assert(right_cons);

			return equal_p_impl(left_cons->car, right_cons->car)
				&& equal_p_impl(left_cons->cdr, right_cons->cdr);
		}
		default:
			// Callables are not comparable unless they are the same
			// object (handled at the start of equal_p_impl)
			return false;
	}

}

result<valuep> equal_p(valuep left, valuep right) {
	return make_number(equal_p_impl(left, right));
}

// TODO: Once we support std::vector<number *> switch over to it
template <typename F, int Init>
result<valuep> arith_fold(std::vector<valuep> params) {
	int result = Init;

	for (auto param : params) {
		auto num = value_cast<number>(param);
		if (!num)
			return fail(error_kind::illegal_argument,
					std::format("expected a number, not {}", *param));

		result = F{}(result, num->val);
	}

	return make_number(result);
}

result<valuep> eval_(std::shared_ptr<environment> env, valuep expr) {
	return eval(expr, env); // TODO: env argument?
}

result<valuep> quote(valuep expr) {
	return expr;
}

result<valuep> quasiquote(std::shared_ptr<environment> env, valuep expr) {
	return quasi_unquote(expr, env);
}

result<valuep> if_(std::shared_ptr<environment> env, valuep condition_expr, valuep consequent, valuep alternate) {
	auto condition = TRY(eval(condition_expr, env));

	if (auto val = value_cast<number>(condition); !val || val->val) {
		return eval(consequent, env);
	} else {
		return eval(alternate, env);
	}
}

result<valuep> lambda_(std::shared_ptr<environment> env, valuep formals, valuep body) {
	return make_lambda(TRY(function_formals::parse(formals)), body, env);
}

result<valuep> define(std::shared_ptr<environment> env, valuep tgt, valuep body) {
	if (auto name = value_cast<symbol>(tgt)) {
		// (define name value)

		auto value = TRY(eval(body, env));
		env->values[name->val] = value;
		return value;
	} else if (auto name_and_formals = value_cast<cons>(tgt)) {
		// (define (name formals...) body)

		auto name = value_cast<symbol>(name_and_formals->car);
		auto formals = name_and_formals->cdr;
		if (!name)
			return fail(error_kind::illegal_argument, std::format(
						"define expects a symbol for name, not {}",
						*name_and_formals->car));

		auto value = make_lambda(TRY(function_formals::parse(formals)), body, env);
		env->values[name->val] = value;
		return value;
	}

	return fail(error_kind::illegal_argument, std::format(
				"define expects either a cons for name and formals, or a symbol for name, not {}",
				*tgt));
}

result<valuep> define_macro(std::shared_ptr<environment> env, cons *name_and_formals, valuep body) {
	auto name = value_cast<symbol>(name_and_formals->car);
	auto formals = name_and_formals->cdr;
	if (!name)
		return fail(error_kind::illegal_argument, std::format(
					"define-macro expects a symbol for name, not {}",
					*name_and_formals->car));

	auto value = make_macro(TRY(function_formals::parse(formals)), body, env);
	env->values[name->val] = value;
	return value;
}

} // namespace builtins

std::shared_ptr<environment> prepare_root_environment() {
	auto root_env = std::make_shared<environment>();

	auto function = [&] (std::string name, auto tgt) {
		root_env->values[name] = make_builtin_procedure(name, tgt);
	};

	auto macro = [&] (std::string name, auto tgt) {
		root_env->values[name] = make_builtin_macro(name, tgt);
	};

	using namespace builtins;

	function("cons", cons_);
	function("car", car);
	function("cdr", cdr);
	function("nil?", nil_p);
	function("eq?", equal_p);

	function("+", arith_fold<std::plus<int>, 0>);
	function("*", arith_fold<std::multiplies<int>, 1>);

	function("eval", eval_);

	macro("quote", quote);
	macro("quasiquote", quasiquote);

	macro("if", if_);
	macro("lambda", lambda_);
	macro("λ", lambda_);

	macro("define", define);
	macro("define-macro", define_macro);

	return root_env;
}
