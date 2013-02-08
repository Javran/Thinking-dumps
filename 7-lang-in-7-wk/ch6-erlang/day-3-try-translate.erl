#!/usr/bin/env escript

-module('day-3-try-translate').
-export([loop/0]).

loop() ->
	receive
		"casa" ->
			u:ws("house"),
			loop();
		"blanca" ->
			u:ws("white"),
			loop();
		":stop" ->
			u:ws("<Stop process>");
			% loop/0 is used to keep process going
			%     we remove `loop()` here, 
			%     so the process will stop
		_ ->
			u:ws("I don't understand."),
			loop()
	end.

main(_) -> 
	c:c(u),
	Pid = spawn(fun ?MODULE:loop/0),
	u:ws("Spawned pid:"),
	u:wl(Pid),

	Pid ! "casa",
	Pid ! "blanca",
	Pid ! "loco",
	Pid ! ":stop",

	% sleep to get the results
	timer:sleep(1000),
	ok.
