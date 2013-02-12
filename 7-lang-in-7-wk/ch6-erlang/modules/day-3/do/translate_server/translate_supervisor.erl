-module(translate_supervisor).
-behavior(supervisor).
-export([
	init/1
]).

-export([
	start_link/0,
	get_service/1
]).

start_link() ->
	supervisor:start_link(?MODULE, []).

get_service(SupRef) ->
	% find the child by id 'translate_service_id'
	{_, Pid, _, _} = hd(
		lists:filter(
			fun({Id, _, _, _}) -> 
				Id == translate_service_id
			end,
			supervisor:which_children(SupRef))
	),
	Pid.

init(_Args) ->
	{ok, {{one_for_one, 10, 1},
		[{translate_service_id, {translate_service, start_link, []},
			permanent, brutal_kill, worker, [translate_service]}]}}.
