#!/usr/bin/env escript

% WARNING: DO NOT try roulette at home!

main(_) ->
	c:c(u),
	code:add_patha("./day-3-modules"),
	c:c("./day-3-modules/roulette"),

	Gun = spawn(fun roulette:loop/0),

	SendNumber = fun(X) ->
		io:format("Sending number ~w to Gun...~n", [X]),
		Gun ! X 
	end,

	% note here number is sent asynchronously
	lists:foreach(SendNumber, [1,2,4,5,6]),
	timer:sleep(100),
	u:ws("Process alive?"),
	u:wl(erlang:is_process_alive(Gun)),
	lists:foreach(SendNumber, [3,1,2,3,4,5,6]),
	timer:sleep(100),
	u:ws("Process alive?"),
	u:wl(erlang:is_process_alive(Gun)),
	ok.
