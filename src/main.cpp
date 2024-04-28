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


#include "result.hpp"
#include "parse.hpp"
#include "token.hpp"
#include "eval.hpp"

#include <iostream>
#include <fstream>
#include <memory>

#include <readline/readline.h>
#include <readline/history.h>


void repl() {
	using_history();

	auto root_env = prepare_root_environment();
	auto env = std::make_shared<environment>();
	env->parent = root_env;

	uint64_t out_ctr = 1;

	while (true) {
		char *input = readline(">>> ");
		if (!input)
			break;

		add_history(input);

		std::istringstream ss{input};

		auto tokens = tokenize(ss);
		auto it = tokens.begin();

		while (true) {
			auto maybe_expr = parse_expr(it);
			if (!maybe_expr && maybe_expr.error().kind == error_kind::end_of_file) {
				break;
			} else if (!maybe_expr) {
				std::cout << "error during parsing of input: "
					  << maybe_expr.error() << "\n";
				break;
			}

			auto expr = MUST(maybe_expr);
			auto maybe_result = eval(expr, env);

			if (!maybe_result) {
				std::cout << "error during evaluation of input: "
					  << maybe_result.error() << "\n";
				break;
			}
			auto result = MUST(maybe_result);

			std::string out_name = std::format("${}", out_ctr++);
			std::cout << out_name << " = " << *result << "\n";
			env->values[out_name] = result;
		}

		free(input);
	}
}


int main(int argc, char **argv) {
	if (argc != 2) {
		repl();
		return 0;
	}

	std::ifstream file{argv[1]};
	if (!file) {
		std::cerr << "Failed to open file\n";
		return 2;
	}

	auto root_env = prepare_root_environment();
	auto env = std::make_shared<environment>();
	env->parent = root_env;

	auto tokens = tokenize(file);

	auto it = tokens.begin();

	while (true) {
		auto maybe_expr = parse_expr(it);
		if (!maybe_expr && maybe_expr.error().kind == error_kind::end_of_file)
			break;

		auto expr = MUST(maybe_expr);

		std::cout << "=> " << *expr << "\n";

		std::cout << "<= " << *MUST(eval(expr, env)) << "\n";
	}
}
