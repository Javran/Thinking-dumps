#!/usr/bin/env escript

mirror(Anything) -> Anything.

% TODO: test loading modules

main(_) ->
	io:format("~w~n",[mirror(smiling_mug)]),
	io:format("~w~n",[mirror(1)]).
