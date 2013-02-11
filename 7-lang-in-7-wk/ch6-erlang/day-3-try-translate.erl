#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	% please run the script in current directory
	%     or change the relative path below accordingly
	c:c("./modules/day-3/try/translate"),

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
