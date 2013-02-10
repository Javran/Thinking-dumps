-module(kitty_server).

-export([
	start_link/0,
	order_cat/4,
	return_cat/2,
	close_shop/1]).

% what is 'record'? never see it before ...
%     seems to descript some structure and link it with term 'cat' ...
-record(cat, {name, color=green, description}).


%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, {order, Name, Color, Description}},
	receive
		{Ref, Cat} ->
			erlang:demonitor(Ref, [flush]),
			Cat;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.

%% Asynchronous call
return_cat(Pid, Cat = #cat{}) ->
	Pid ! {return, Cat},
	ok.

%% Synchronous call
close_shop(Pid) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, terminate},
	receive
		{Ref, ok} ->
			erlang:demonitor(Ref, [flush]),
			ok;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.

%%% Server functions
init() -> loop([]).
loop(Cats) ->
	receive
		{Pid, Ref, {order, Name, Color, Description}} ->
			if 
				Cats =:= [] ->
					Pid ! {Ref, make_cat(Name, Color, Description)},
					loop(Cats);
				Cats =/= [] ->
					Pid ! {Ref, hd(Cats)},
					loop(tl(Cats))
			end;

		{return, Cat = #cat{}} ->
			loop([Cat|Cats]);
		{Pid, Ref, terminate} ->
			Pid ! {Ref, ok},
			terminate(Cats);
		Unknown ->
			io:format("Unknown message: ~p~n", [Unknown]),
			loop(Cats)
	end.

%%% private functions
make_cat(Name, Color, Description) ->
	#cat{name=Name, color=Color, description=Description}.

terminate(Cats) ->
	io:format("Terminating ...~p~n",[Cats]),
	lists:foreach( 
		fun(C) -> 
			io:format("~p was set free. ~n", [C#cat.name])
		end,
		Cats),
	ok.
