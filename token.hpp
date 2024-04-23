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

#include <generator>
#include <istream>
#include <variant>
#include <string>


struct raw_symbol {
	std::string value;
};

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
std::ostream &operator<<(std::ostream &os, const token &tok);

std::generator<token> tokenize(std::istream &input);
// std::generator<token>::iterator is exposition-only...
// If the mountain will not come to Mahomet, Mahomet must go to the moutain
using token_iter = decltype(std::declval<std::generator<token>>().begin());
