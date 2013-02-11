-module(kitty_server).

-export([
	start_link/0,
	order_cat/4,
	return_cat/2,
	close_shop/1]).

% records are used to support named accessing, the link below would help:
%     http://20bits.com/article/erlang-an-introduction-to-records
-record(cat, {name, color=green, description}).


%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	% please refer to
	%     http://www.erlang.org/doc/man/erlang.html#monitor-2
	% enabling monitor will cause 'DOWN' messages sent
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

%% Asynchronous call
return_cat(Pid, Cat = #cat{}) ->
	Pid ! {return, Cat},
	ok.

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
					% goes without saying that (hd,tl) = (head,tail)
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
	io:format("Terminating ...~n"),
	lists:foreach( 
		fun(C) -> 
			io:format("~p was set free. ~n", [C#cat.name])
		end,
		Cats),
	ok.
