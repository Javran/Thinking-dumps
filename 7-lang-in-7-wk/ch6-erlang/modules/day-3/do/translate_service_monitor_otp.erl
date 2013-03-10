-module(translate_service_monitor_otp).
-behavior(supervisor).
-export([
	start_link/0,
	init/1]).

start_link() ->
	supervisor:start_link({local, monitor}, ?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 10, 1},
		[{translator, {translate_service_otp, start_link, [translator]},
			permanent, brutal_kill, worker, [translate_service_otp]}]}}.
