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


#pragma once

#include "result.hpp"
#include "util.hpp"

#include <unordered_map>
#include <algorithm>
#include <ostream>
#include <sstream>
#include <variant>
#include <memory>
#include <string>
#include <vector>


struct value;
using valuep = std::shared_ptr<value>;


// TODO: I think this implementation of environment lets you leak memory due to the
// interaction between lambdas and environments, by forming reference cycles.

struct environment : std::enable_shared_from_this<environment> {
	std::unordered_map<std::string, valuep> values;
	std::shared_ptr<environment> parent;

	result<valuep> lookup(std::string name) {
		auto in = shared_from_this();

		while (in) {
			auto it = in->values.find(name);
			if (it != in->values.end())
				return it->second;

			in = in->parent;
		}

		return fail(error_kind::unbound_variable, name);
	}
};


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

inline valuep make_symbol(std::string sym) {
	return std::make_shared<value>(symbol{std::move(sym)});
}

inline valuep make_number(int num) {
	return std::make_shared<value>(number{num});
}

inline valuep make_cons(valuep car, valuep cdr) {
	return std::make_shared<value>(cons{std::move(car), std::move(cdr)});
}

inline valuep make_lambda(function_formals formals, valuep body, std::shared_ptr<environment> captured_environment) {
	return std::make_shared<value>(lambda{std::move(formals), std::move(body), false, std::move(captured_environment)});
}

inline valuep make_macro(function_formals formals, valuep body, std::shared_ptr<environment> captured_environment) {
	return std::make_shared<value>(lambda{std::move(formals), std::move(body), true, std::move(captured_environment)});
}

inline valuep make_functionlike_builtin(std::string name, builtin::fn_type tgt) {
	return std::make_shared<value>(builtin{function_formals{}, std::move(name), true, tgt});
}

inline valuep make_macrolike_builtin(std::string name, builtin::fn_type tgt) {
	return std::make_shared<value>(builtin{function_formals{}, std::move(name), false, tgt});
}

inline valuep make_nil() {
	return std::make_shared<value>(nil{});
}


template <typename Ty>
Ty *value_cast(value *v) {
	return std::get_if<Ty>(v);
}

template <typename Ty>
Ty *value_cast(const valuep &v) {
	return std::get_if<Ty>(v.get());
}
