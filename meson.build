project('lisp', 'cpp',
	version : '0.1',
	license : 'GPL-3.0-or-later',
	default_options : ['warning_level=2',
			   'cpp_std=gnu++23'])

readline = dependency('readline')

exe = executable('lisp',
		 'src/main.cpp',
		 'src/token.cpp',
		 'src/parse.cpp',
		 'src/value.cpp',
		 'src/eval.cpp',
		 install : true,
		 dependencies : readline)

subdir('tests')
