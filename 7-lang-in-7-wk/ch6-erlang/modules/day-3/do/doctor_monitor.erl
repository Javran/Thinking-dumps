-module(doctor_monitor).
-export([start/0]).

start() ->
	loop(0).

loop(DoctorPid) ->
	process_flag(trap_exit, true),
	receive
		new ->
			u:ws("Monitor: creating doctor..."),
			register(doctor, spawn_link(doctor, start, [])),
			doctor ! new,
			doctor ! {monitor, self()},
			self() ! {doctor, whereis(doctor)},
			loop(0);

		{doctor, NewDoctorPid} ->
			io:format("Monitor: set doctor: ~p~n", [NewDoctorPid]),
			loop(NewDoctorPid);

		% raise an error for test
		kill ->
			exit(monitor_die);

		% restart doctor if it dies
		{'EXIT', DoctorPid, Reason} ->
			io:format("Monitor: the doctor ~p died with reason ~p.~n",
				[DoctorPid, Reason]),
			u:ws("Monitor: restarting doctor..."),
			self() ! new,
			% the state will be replaced when 'new' got processed
			loop(0)
	end.
