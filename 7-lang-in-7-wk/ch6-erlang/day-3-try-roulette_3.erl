#!/usr/bin/env escript

main(_) ->
	c:c(u),
	code:add_patha("./day-3-modules"),
	c:c("./day-3-modules/roulette"),
	c:c("./day-3-modules/doctor"),

	Doctor = spawn(fun doctor:loop/0),

	Doctor ! new,

	SendNumber = fun(X) ->
		% sleep for a while so Doctor can bring back revolver
		timer:sleep(100),
		io:format("Sending number ~w to revolver...~n", [X]),
		revolver ! X 
	end,

	% note here messages are sent asynchronously
	lists:foreach(SendNumber, [1,2,3,4,5,6]),

	Doctor ! {stop, self()},
	receive
		stopped ->
			u:ws("<Doctor stopped>")
	end.
