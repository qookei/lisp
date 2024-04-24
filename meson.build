project('lisp', 'cpp',
	version : '0.1',
	license : 'GPL-3.0-or-later',
	default_options : ['warning_level=2',
			   'cpp_std=gnu++23'])

exe = executable('lisp',
		 'src/main.cpp',
		 'src/token.cpp',
		 'src/parse.cpp',
		 'src/eval.cpp',
		 install : true)

subdir('tests')
