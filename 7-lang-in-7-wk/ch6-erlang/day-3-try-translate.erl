#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	% please run the script in current directory
	%     or change the relative path below accordingly
	code:add_patha("./day-3-modules"),
	c:c("./day-3-modules/translate"),

	Pid = spawn(fun translate:loop/0),
	u:ws("Spawned pid:"),
	u:wl(Pid),

	Pid ! "casa",
	Pid ! "blanca",
	Pid ! "loco",
	Pid ! ":stop",

	% sleep to get the results
	timer:sleep(1000),
	ok.
