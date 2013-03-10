#!/usr/bin/env escript

mirror(Anything) -> Anything.

main(_) ->
	c:c(u),
	u:wl(mirror(smiling_mug)),
	u:wl(mirror(1)).
