-module(translate_supervisor).
-behavior(supervisor).
-export([
	init/1
]).

-export([
	start_link/1,
	get_service/1,
	get_tcp_service/1,
	stop/1
]).

start_link(Port) ->
	supervisor:start_link({local, supervisor_inst}, ?MODULE, [Port]).

stop(SupRef) ->
	lists:foreach(
		fun({Id, _, _, _}) ->
			supervisor:terminate_child(SupRef, Id)
		end,
		supervisor:which_children(SupRef)).

find_child(SupRef, TargetId) ->
	% find the child by Id
	% might fail here
	{_, Pid, _, _} = hd(
		lists:filter(
			fun({Id, _, _, _}) -> 
				Id == TargetId
			end,
			supervisor:which_children(SupRef))
	),
	Pid.

get_service(SupRef) ->
	find_child(SupRef, translate_service_id).
	
get_tcp_service(SupRef) ->
	find_child(SupRef, translate_tcp_id).


init(_Args) ->
	[Port] = _Args,
	{ok, {{one_for_one, 10, 1},
		[
			{translate_service_id, {translate_service, start_link, []},
				permanent, 5000, worker, [translate_service]},
			{translate_tcp_id, {translate_tcp, start_link, [Port]},
				permanent, 5000, worker, [translate_tcp]}	
		]}}.
