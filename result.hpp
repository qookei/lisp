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

#include <expected>
#include <iostream>
#include <string>


enum class error_kind {
	syntax_error,
	end_of_file,
	unrecognized_form,
	unbound_variable,
	illegal_argument,
};

// TODO: perhaps error should be a lisp value? so later we can provide
// a way to catch errors in lisp code

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

template <typename ...Ts>
std::unexpected<error> fail(Ts &&...ts) {
	return std::unexpected{error{std::forward<Ts>(ts)...}};
}


namespace {

template<typename T>
T value_or_void(result<T> &ex) {
	if constexpr (!std::is_same_v<T, void>)
		return std::move(ex.value());
}

[[noreturn, maybe_unused]] inline void abort_due_to_error(const error &err) {
	std::cerr << "aborting due to error: " << err << "\n";
	abort();
}

} // namespace anonymous


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
