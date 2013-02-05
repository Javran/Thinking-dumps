#!/usr/bin/env escript

mirror(Anything) -> Anything.

% TODO: test loading modules

main(_) ->
	c:c(u),
	u:writeln(mirror(smiling_mug)),
	u:writeln(mirror(1)).
