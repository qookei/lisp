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


#include "token.hpp"
#include "util.hpp"

#include <string_view>
#include <cassert>


// TODO: quotes can appear inside of symbols
inline constexpr std::string_view SPECIAL_CHARACTERS = "()'`,";

std::generator<token> tokenize(std::istream &input) {
	auto valid_symbol_chr = [] (char c) {
		return !SPECIAL_CHARACTERS.contains(c) && !isspace(c);
	};

	while (!input.eof()) {
		char c = input.get();

		if (isspace(c)) {
			continue;
		} else if (c == ';') {
			while (!input.eof() && input.get() != '\n')
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
			if (input.peek() == '@') {
				input.get();
				co_yield unquote_splicing{};
			} else {
				co_yield unquote{};
			}
		} else {
			std::string v{};

			assert(valid_symbol_chr(c));

			while (valid_symbol_chr(c) && !input.eof()) {
				v.push_back(c);
				c = input.get();
			}

			// Return the last character since it's not a
			// part of the symbol
			if (!input.eof() && !valid_symbol_chr(c)) {
				input.unget();
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
