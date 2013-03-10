-module(translate_service_monitor).
-export([loop/0]).

loop() ->
	process_flag(trap_exit, true),
	receive
		new ->
			u:ws("Creating and monitoring process."),
			register(translator, spawn_link(fun translate_service:init/0)),
			loop();
		{'EXIT', From, Reason} ->
			io:format("The translator ~p died with reason ~p.~n",
				[From, Reason]),
			u:ws("Restarting."),
			self() ! new,
			loop()
	end.
