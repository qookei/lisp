pass_tests = [
  'basic',
  'macro',
  'quasi',
  'lambda-env',
]

fail_tests = [
  'quasi-bad1',
  'quasi-bad2',
]

all_tests = pass_tests + fail_tests
foreach test_case : all_tests
  test(test_case, exe,
       args : [files(test_case + '.lisp')],
       should_fail : fail_tests.contains(test_case))
endforeach
