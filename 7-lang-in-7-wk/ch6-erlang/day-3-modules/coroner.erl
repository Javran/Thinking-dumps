-module(coroner).
-export([loop/0]).

loop() ->
	% please refer to `erl -man erlang`
	%     to see what the below line will do.
	process_flag(trap_exit, true),
	receive
		{monitor, Process} ->
			link(Process),
			io:format("Monitoring process ~p...~n", [Process]),
			loop();
		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p.~n",
				[From, Reason]),
			u:ws("Start another one."),
			loop();
		{stop, From} ->
			u:ws("<Coroner stopping>"),
			From ! stopped
	end.
