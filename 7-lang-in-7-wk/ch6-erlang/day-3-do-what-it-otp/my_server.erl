-module(my_server).
-compile(export_all).


call(Pid, Msg) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, Msg},
	receive
		{Ref, Reply} ->
			erlang:demonitor(Ref, [flush]),
			Reply;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.
