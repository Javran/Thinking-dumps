-module(doctor).
-export([loop/0]).

loop() ->
	process_flag(trap_exit, true),
	receive
		new ->
			u:ws("Creating and monitoring process."),
			register(revolver, spawn_link(fun roulette:loop/0)),
			loop();
		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p.~n",
				[From, Reason]),
			u:ws("Restarting."),
			self() ! new,
			loop();
		{stop, From} ->
			u:ws("<Doctor stopping>"),
			From ! stopped
	end.
