#!/usr/bin/env escript

main(_) ->
	c:c(u),
	c:c("./modules/day-3/try/roulette"),
	c:c("./modules/day-3/try/coroner"),

	Revolver = spawn(fun roulette:loop/0),
	Coroner = spawn(fun coroner:loop/0),

	SendNumber = fun(X) ->
		io:format("Sending number ~w to Revolver...~n", [X]),
		Revolver ! X 
	end,

	% note here messages are sent asynchronously
	Coroner ! {monitor, Revolver},
	lists:foreach(SendNumber, [1,2,3,4,5,6]),

	Coroner ! {stop, self()},
	receive
		stopped ->
			u:ws("<Coroner stopped>")
	end.
