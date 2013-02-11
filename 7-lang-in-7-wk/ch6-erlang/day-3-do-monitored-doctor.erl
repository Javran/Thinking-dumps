#!/usr/bin/env escript

main(_) ->
	c:c(u),
	c:c("./modules/day-3/do/roulette"),
	c:c("./modules/day-3/do/doctor"),
	c:c("./modules/day-3/do/doctor_monitor"),

	SendNumber = fun(X) ->
		% sleep for a while so Doctor can bring back revolver
		timer:sleep(100),
		io:format("Sending number ~w to revolver...~n", [X]),
		revolver ! X 
	end,

	PlayGame = fun() ->
		lists:foreach(SendNumber, [1,2,3,4,5,6]),
		timer:sleep(100)
	end,

	Monitor = spawn(doctor_monitor, start, []), 
	% quick and dirty ... need to register monitor manually
	register(monitor, Monitor),

	CheckState = fun() ->
		State = lists:map(
			fun(X) -> 
				if
					is_pid(X) -> is_process_alive(X);
					true -> is_process_alive(whereis(X))
				end
			end,
			[revolver, doctor, monitor]),

		io:format(
			">>> Checking if revolver, doctor and monitor are all alive ... ~p <<<~n",
			[lists:all(fun(X) -> X end, State)])
	end,

	monitor ! new,
	timer:sleep(100),

	CheckState(),
	PlayGame(),

	% try to kill doctor
	doctor ! kill,
	timer:sleep(100),

	CheckState(),
	PlayGame(),

	Monitor ! kill,
	timer:sleep(100),

	CheckState(),
	PlayGame(),

	% try to kill doctor again to see if monitor works
	doctor ! kill,
	timer:sleep(100),

	CheckState(),
	ok.
