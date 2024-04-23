project('lisp', 'cpp',
	version : '0.1',
	license : 'GPL-3.0-or-later',
	default_options : ['warning_level=2',
			   'cpp_std=gnu++23'])

exe = executable('lisp',
		 'lisp.cpp',
		 'token.cpp',
		 install : true)

test('basic', exe)
