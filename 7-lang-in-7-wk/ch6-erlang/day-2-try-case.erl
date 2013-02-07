#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	u:wl(testCase("dog")),
	% underdog
	u:wl(testCase("cat")),
	% thundercat
	u:wl(testCase("elephant")),
	% dumbo
	u:wl(testCase("wolf")),
	% something else
	ok.

testCase(Animal) ->
	case Animal of
		"dog" -> underdog;
		"cat" -> thundercat;
		"elephant" -> dumbo;
		_ -> something_else
	end.
