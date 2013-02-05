#!/usr/bin/env escript

main(_) -> 
	c:c('day-1-try-module'),
	'day-1-try-module':module_test(),
	'day-1-try-module':writeln("call module function to show message"),
	io:format("print things like this", []).
