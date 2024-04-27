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


#include "value.hpp"

#include <cassert>
#include <format>
#include <ranges>


result<function_formals> function_formals::parse(valuep formals) {
	if (formals->type() == value_type::nil) {
		return function_formals{};
	} else if (auto formal_sym = value_cast<symbol>(formals)) {
		return function_formals{{item{formal_sym->val, true}}};
	} else if (formals->type() != value_type::cons) {
		return fail(error_kind::unrecognized_form,
				std::format("formals should be a list or a symbol, not {}",
						*formals));
	}

	std::vector<item> items;

	auto cur = value_cast<cons>(formals);
	assert(cur);

	while (cur) {
		if (auto car_sym = value_cast<symbol>(cur->car)) {
			items.push_back({car_sym->val, false});
		} else {
			return fail(error_kind::unrecognized_form,
					std::format("formal should be a symbol, not {}",
							*cur->car));
		}

		if (auto cdr_sym = value_cast<symbol>(cur->cdr)) {
			items.push_back({cdr_sym->val, true});
			break;
		} else if (cur->cdr->type() != value_type::cons
				&& cur->cdr->type() != value_type::nil) {
			return fail(error_kind::unrecognized_form,
				std::format("formals should be a list or a symbol, not {}",
						*cur->cdr));
		}

		cur = value_cast<cons>(cur->cdr);
	}

	return function_formals{std::move(items)};
}

std::ostream &operator<<(std::ostream &os, const function_formals &self) {
	// Print "foo" instead of "(. foo)"
	if (self.items.size() == 1 && self.items[0].rest)
		return os << self.items[0].name;

	os << "(";

	for (const auto &[index, item] : std::views::enumerate(self.items)) {
		if (item.rest)
			os << ". ";

		os << item.name;

		if (std::size_t(index) != self.items.size() - 1)
			os << " ";
	}

	return os << ")";
}
