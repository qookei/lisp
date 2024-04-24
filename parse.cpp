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


#include "parse.hpp"

#include <string_view>
#include <algorithm>
#include <cctype>


// TODO: end-of-file in the middle of an expression should be transformed into syntax_error
struct parser {
	result<valuep> operator()(raw_symbol sym) {
		auto is_digit = [] (unsigned char c) { return ::isdigit(c); };

		if ((sym.value[0] == '-' && sym.value.size() > 1 && std::ranges::all_of(sym.value.substr(1), is_digit))
				|| std::ranges::all_of(sym.value, is_digit))
			return std::make_shared<value>(number{std::stoi(sym.value)});

		return std::make_shared<value>(symbol{std::move(sym.value)});
	}

	result<valuep> wrapped_expr(std::string_view name) {
		return make_cons(
			make_symbol(std::string{name}),
			make_cons(TRY(parse_expr()), make_nil()));
	}

	result<valuep> operator()(quote) {
		return wrapped_expr("quote");
	}

	result<valuep> operator()(quasiquote) {
		return wrapped_expr("quasiquote");
	}

	result<valuep> operator()(unquote) {
		return wrapped_expr("unquote");
	}

	result<valuep> operator()(unquote_splicing) {
		return wrapped_expr("unquote-splicing");
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

	token_iter &it;
};

result<valuep> parse_expr(token_iter &it) {
	return parser{it}.parse_expr();
}
