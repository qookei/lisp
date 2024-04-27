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
#include <concepts>
#include <cassert>
#include <ostream>
#include <sstream>
#include <utility>
#include <variant>
#include <memory>
#include <string>
#include <vector>


enum class value_type {
	symbol,
	number,
	cons,
	nil,
	lambda,
	builtin
};


struct value {
	virtual ~value() = default;

	value_type type() const {
		return ty_;
	}

	friend std::ostream &operator<<(std::ostream &os, const value &v) {
		return v.format(os);
	}

	friend std::ostream &operator<<(std::ostream &os, const value *v) {
		return v->format(os);
	}

protected:
	value(value_type ty)
	: ty_{ty} { }

	virtual std::ostream &format(std::ostream &os) const = 0;

private:
	value_type ty_;
};
using valuep = std::shared_ptr<value>;


template <>
struct std::formatter<value, char> {
	template<class Ctx>
	constexpr Ctx::iterator parse(Ctx &ctx) {
		return ctx.begin();
	}

	template<class Ctx>
	Ctx::iterator format(const value &val, Ctx& ctx) const {
		std::ostringstream os;
		os << val;

		return std::ranges::copy(std::move(os).str(), ctx.out()).out;
	}
};


template <typename T>
concept value_subtype =
	std::derived_from<T, value>
	&& requires {
		{ T::this_type } -> std::convertible_to<value_type>;
	};

template <value_subtype T>
T *value_cast(value *v) {
	if (v->type() != T::this_type)
		return nullptr;

	return static_cast<T *>(v);
}

// TODO: Maybe this should return std::shared_ptr<T>?
template <typename T>
T *value_cast(const valuep &v) {
	return value_cast<T>(v.get());
}


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


struct symbol : value {
	static inline constexpr value_type this_type = value_type::symbol;

	symbol(std::string v)
	: value{this_type}, val{std::move(v)} { }

	virtual std::ostream &format(std::ostream &os) const override {
		return os << val;
	}

	const std::string val;
};
static_assert(value_subtype<symbol>);

inline valuep make_symbol(std::string sym) {
	return std::make_shared<symbol>(std::move(sym));
}


struct number : value {
	static inline constexpr value_type this_type = value_type::number;

	number(int v)
	: value{this_type}, val{v} { }

	virtual std::ostream &format(std::ostream &os) const override {
		return os << val;
	}

	const int val;
};
static_assert(value_subtype<number>);

inline valuep make_number(int num) {
	return std::make_shared<number>(num);
}


struct cons : value {
	static inline constexpr value_type this_type = value_type::cons;

	cons(valuep car, valuep cdr)
	: value{this_type}, car{std::move(car)}, cdr{std::move(cdr)}  { }

	virtual std::ostream &format(std::ostream &os) const override {
		os << "(" << *car;

		auto cur = cdr;

		while (cur) {
			if (cur->type() == value_type::cons) {
				auto kons = std::static_pointer_cast<cons>(cur);
				os << " " << *kons->car;
				cur = kons->cdr;
			} else if (cur->type() == value_type::nil) {
				return os << ")";
			} else {
				// Improper list or single cons
				return os << " . " << *cur << ")";
			}
		}

		std::unreachable();
	}

	valuep car;
	valuep cdr;
};
static_assert(value_subtype<cons>);

inline valuep make_cons(valuep car, valuep cdr) {
	return std::make_shared<cons>(std::move(car), std::move(cdr));
}


struct nil : value {
	static inline constexpr value_type this_type = value_type::nil;

	nil() : value{this_type} { }

	virtual std::ostream &format(std::ostream &os) const override {
		return os << "()";
	}
};
static_assert(value_subtype<nil>);

inline valuep make_nil() {
	return std::make_shared<nil>();
}


struct function_formals {
	struct item {
		std::string name;
		bool rest;
	};

	std::vector<item> items{};

	static result<function_formals> parse(valuep formals);

	template <typename F>
	result<void> map_params(valuep params, F &&fn) const {
		auto item_it = items.begin();
		auto cur = params;

		while (item_it != items.end() && cur->type() == value_type::cons) {
			const auto &item = *item_it;
			if (item.rest) {
				++item_it;
				assert(item_it == items.end());

				TRY(fn(item.name, true, cur));
				return {};
			}

			auto cur_cons = value_cast<cons>(cur);
			assert(cur_cons);

			TRY(fn(item.name, false, cur_cons->car));

			cur = cur_cons->cdr;
			item_it++;
		}

		if (params->type() != value_type::nil && params->type() != value_type::cons) {
			return fail(error_kind::unrecognized_form,
					std::format("parameters should be a list, not {}",
							*params));
		} else if (cur->type() == value_type::cons) {
			return fail(error_kind::unrecognized_form,
					std::format("too many parameters, left over {}", *cur));
		} else if (item_it != items.end()) {
			return fail(error_kind::unrecognized_form,
					std::format("not enough parameters, first missing paramer is {}",
							item_it->name));
		}

		return {};
	}

	friend std::ostream &operator<<(std::ostream &os, const function_formals &self);
};

struct lambda : value {
	static inline constexpr value_type this_type = value_type::lambda;

	lambda(function_formals formals, valuep body, bool macro,
			std::shared_ptr<environment> captured_environment)
	: value{this_type}, formals{std::move(formals)}, body{std::move(body)}, is_macro{macro},
		captured_environment{std::move(captured_environment)} { }

	virtual std::ostream &format(std::ostream &os) const override {
		return os << (is_macro ? "[macro " : "[lambda ")
				<< static_cast<const void *>(this) << " " << formals
				<< " (env " << captured_environment.get() << ")]";
	}

	function_formals formals;
	valuep body;

	bool is_macro;

	std::shared_ptr<environment> captured_environment;
};
static_assert(value_subtype<lambda>);

inline valuep make_lambda(function_formals formals, valuep body, std::shared_ptr<environment> captured_environment) {
	return std::make_shared<lambda>(std::move(formals), std::move(body), false, std::move(captured_environment));
}

inline valuep make_macro(function_formals formals, valuep body, std::shared_ptr<environment> captured_environment) {
	return std::make_shared<lambda>(std::move(formals), std::move(body), true, std::move(captured_environment));
}


struct builtin : value {
	static inline constexpr value_type this_type = value_type::builtin;
	using fn_type = result<valuep> (*)(std::vector<valuep> params, std::shared_ptr<environment> env);

	builtin(function_formals formals, std::string name, bool evaluate_params, fn_type tgt)
	: value{this_type}, formals{std::move(formals)}, name{std::move(name)},
		evaluate_params{evaluate_params}, tgt{tgt} { }

	virtual std::ostream &format(std::ostream &os) const override {
		return os << (evaluate_params ? "[built-in macro " : "[built-in procedure ")
				<< name << " " << formals << "]";
	}

	function_formals formals;

	std::string name;


	bool evaluate_params;
	fn_type tgt;
};
static_assert(value_subtype<builtin>);

inline valuep make_functionlike_builtin(std::string name, function_formals formals, builtin::fn_type tgt) {
	return std::make_shared<builtin>(std::move(formals), std::move(name), true, tgt);
}

inline valuep make_macrolike_builtin(std::string name, function_formals formals, builtin::fn_type tgt) {
	return std::make_shared<builtin>(std::move(formals), std::move(name), false, tgt);
}
