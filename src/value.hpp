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
	callable
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


template <typename F>
result<valuep> map(valuep list, F &&fn);


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

	template <typename FE, typename FP>
	result<void> map_params(valuep params, FE &&eval, FP &&process) const {
		auto item_it = items.begin();
		auto cur = params;

		while (item_it != items.end() && cur->type() == value_type::cons) {
			const auto &item = *item_it;
			if (item.rest) {
				++item_it;
				assert(item_it == items.end());

				TRY(process(item.name, true, TRY(map(cur, std::forward<FE>(eval)))));
				return {};
			}

			auto cur_cons = value_cast<cons>(cur);
			assert(cur_cons);

			TRY(process(item.name, false, TRY(eval(cur_cons->car))));

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
					std::format("not enough parameters, first missing parameter is {}",
							item_it->name));
		}

		return {};
	}

	friend std::ostream &operator<<(std::ostream &os, const function_formals &self);
};


struct callable : value {
	static inline constexpr value_type this_type = value_type::callable;

	callable(function_formals formals, std::string name, bool macrolike)
	: value{this_type}, formals{std::move(formals)}, name{std::move(name)}, macrolike{macrolike} { }

	virtual std::ostream &format(std::ostream &os) const override {
		return os << (macrolike ? "[macro" : name.size() ? "[procedure" : "[lambda")
			  << (name.size() ? " " : "") << name << " " << formals << "]";
	}

	result<valuep> apply(valuep params, std::shared_ptr<environment> env) const;

	function_formals formals;
	std::string name;
	bool macrolike;

protected:
	virtual result<valuep> do_apply(std::vector<valuep> params, std::shared_ptr<environment> env) const = 0;
};
static_assert(value_subtype<callable>);


struct lambda final : callable {
	lambda(function_formals formals, valuep body, bool macrolike,
			std::shared_ptr<environment> captured_environment)
	: callable{std::move(formals), "", macrolike}, body{std::move(body)},
		captured_environment{std::move(captured_environment)} { }

	virtual result<valuep> do_apply(std::vector<valuep> params, std::shared_ptr<environment>) const override;

	valuep body;
	std::shared_ptr<environment> captured_environment;
};

inline valuep make_lambda(function_formals formals, valuep body, std::shared_ptr<environment> env) {
	return std::make_shared<lambda>(std::move(formals), std::move(body), false, std::move(env));
}

inline valuep make_macro(function_formals formals, valuep body, std::shared_ptr<environment> env) {
	return std::make_shared<lambda>(std::move(formals), std::move(body), true, std::move(env));
}


template <typename T>
struct builtin_formals_item {
	function_formals::item operator()() const {
		return {"_", false};
	}
};

// TODO: Support vector<T> arguments where T is a value subtype
// TODO: vector<valuep> should be last, since it's the cdr of an improper formals list
template <>
struct builtin_formals_item<std::vector<valuep>> {
	function_formals::item operator()() const {
		return {"_", true};
	}
};


template <bool WantEnv, typename ...Ts>
struct builtin_cb_type {
	using type = result<valuep> (*)(std::shared_ptr<environment> env, Ts...);
};

template <typename ...Ts>
struct builtin_cb_type<false, Ts...> {
	using type = result<valuep> (*)(Ts...);
};


template <bool WantEnv, typename ...Ts>
struct builtin final : callable {
	using fn_type = typename builtin_cb_type<WantEnv, Ts...>::type;

	builtin(std::string name, bool macrolike, fn_type fn)
	: callable{function_formals{{builtin_formals_item<Ts>{}()...}},
		std::move(name), macrolike}, fn{fn} { }

	template <std::size_t I>
	result<std::tuple_element_t<I, std::tuple<Ts...>>> process_param(const std::vector<valuep> &params) const {
		using tgt_type = std::tuple_element_t<I, std::tuple<Ts...>>;

		if (I >= params.size()) {
			// TODO: If formals are an improper list, "need" value is invalid (we
			// actually need at least sizeof...(Ts) - 1 parameters)
			return fail(error_kind::unrecognized_form, std::format(
						"not enough parameters for {} (given {}, need {})",
						callable::name, params.size(), sizeof...(Ts)));
		}

		if constexpr (std::is_same_v<tgt_type, valuep>) {
			return params[I];
		} else if constexpr (std::is_pointer_v<tgt_type>
				&& value_subtype<std::remove_pointer_t<tgt_type>>) {
			auto tgt = value_cast<std::remove_pointer_t<tgt_type>>(params[I]);
			if (!tgt) {
				return fail(error_kind::illegal_argument, std::format(
							"{} did not expect {} as it's {}{} parameter",
							callable::name, *params[I], I + 1,
							((I + 1) % 10) == 1 ? "st" :
							((I + 1) % 10) == 2 ? "nd" : "th"));
			}

			return tgt;
		} else {
			// TODO: Assuming this is std::vector<valuep>
			tgt_type out;
			valuep cur = params[I];

			while (cur->type() == value_type::cons) {
				auto cur_cons = value_cast<cons>(cur);
				assert(cur_cons);

				out.push_back(cur_cons->car);

				cur = cur_cons->cdr;
			}

			if (cur->type() != value_type::nil) {
				return fail(error_kind::unrecognized_form, std::format(
							"in {} invocation: parameters are not a proper list: {}",
							callable::name, *cur));
			}

			return out;
		}
	}

	result<std::tuple<>> build_fn_args_tup(const std::vector<valuep> &, std::index_sequence<>) const {
		return std::make_tuple();
	}

	template <std::size_t I, std::size_t ...Is>
	result<std::tuple<
		std::tuple_element_t<I, std::tuple<Ts...>>,
		std::tuple_element_t<Is, std::tuple<Ts...>>...>>
	build_fn_args_tup(const std::vector<valuep> &params, std::index_sequence<I, Is...>) const {
		return std::tuple_cat(
			std::make_tuple(TRY(process_param<I>(params))),
			TRY(build_fn_args_tup(params, std::index_sequence<Is...>{})));
	}

	virtual result<valuep> do_apply(std::vector<valuep> params, std::shared_ptr<environment> env) const override {
		auto params_tup = TRY(build_fn_args_tup(params, std::index_sequence_for<Ts...>{}));

		if constexpr (WantEnv) {
			return std::apply(fn, std::tuple_cat(std::make_tuple(env), params_tup));
		} else {
			return std::apply(fn, params_tup);
		}
	}

	fn_type fn;
};


template <typename ...Ts>
struct arg_types_to_builtin_type {
	using type = builtin<false, Ts...>;
};

template <typename ...Ts>
struct arg_types_to_builtin_type<std::shared_ptr<environment>, Ts...> {
	using type = builtin<true, Ts...>;
};


template <typename ...Ts>
inline valuep make_builtin_procedure(std::string name, result<valuep> (*tgt)(Ts...)) {
	using builtin_ty = typename arg_types_to_builtin_type<Ts...>::type;
	static_assert(std::same_as<decltype(tgt), typename builtin_ty::fn_type>);

	return std::make_shared<builtin_ty>(std::move(name), false, tgt);
}

template <typename ...Ts>
inline valuep make_builtin_macro(std::string name, result<valuep> (*tgt)(Ts...)) {
	using builtin_ty = typename arg_types_to_builtin_type<Ts...>::type;
	static_assert(std::same_as<decltype(tgt), typename builtin_ty::fn_type>);

	return std::make_shared<builtin_ty>(std::move(name), true, tgt);
}


template <typename F>
result<valuep> map(valuep list, F &&fn) {
	if (value_cast<nil>(list)) {
		return make_nil();
	} else if (auto kons = value_cast<cons>(list)) {
		return make_cons(
			TRY(fn(kons->car)), TRY(map(kons->cdr, std::forward<F>(fn))));
	} else {
		return fail(error_kind::illegal_argument, std::format("in map: {} is not cons or nil", *list));
	}
}
