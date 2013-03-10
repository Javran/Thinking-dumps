#!/usr/bin/env escript

main(_) -> 
	c:c('./modules/day-1/try/test_module'),
	test_module:module_test(),
	test_module:writeln("call module function to show message"),
	ok.
