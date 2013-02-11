-module(doctor).
-export([start/0]).

start() ->
	loop({0,0}).

loop({MonitorPid, RevolverPid}) ->
	process_flag(trap_exit, true),
	receive
		new ->
			u:ws("Doctor: creating and monitoring process ..."),
			register(revolver, spawn_link(fun roulette:loop/0)),
			loop({MonitorPid,whereis(revolver)});

		new_monitor ->
			NewMonitorPid = spawn_link(doctor_monitor, start, []),
			self() ! {monitor, NewMonitorPid},
			NewMonitorPid ! {doctor, self()},
			loop({0, RevolverPid});

		kill ->
			exit(doctor_die);

		{monitor, NewMonitorPid} ->
			io:format("Doctor: set monitor: ~p~n", [NewMonitorPid]),
			loop({NewMonitorPid, RevolverPid});

		{'EXIT', RevolverPid, Reason} ->
			io:format("Doctor: the shooter ~p died with reason ~p.~n",
				[RevolverPid, Reason]),
			u:ws("Doctor: restarting revolver..."),
			self() ! new,
			loop({MonitorPid, 0});
		{'EXIT', MonitorPid, Reason} ->
			io:format("Doctor: the monitor ~p died with reason ~p.~n",
				[MonitorPid, Reason]),
			u:ws("Doctor: restarting monitor..."),
			self() ! new_monitor,
			loop({0, RevolverPid})
	end.
