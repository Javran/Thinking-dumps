-module(translate_service_otp).

-behavior(gen_server).

-export([
	start_link/1,
	translate/2,
	test_error/1]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

%%% Client API
start_link(Term) -> 
	io:format("Initializing '~p'...~n", [Term]),
	gen_server:start_link({local, Term}, ?MODULE, [], []).

%% Synchronous call
translate(Pid, OriginWord) ->
	gen_server:call(Pid, OriginWord).

%% Synchronous call
test_error(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}.

handle_call(terminate, _From, State) ->
	{stop, normal, ok, State};
handle_call(OriginWord, _From, State) ->
	{reply, translate_core(OriginWord), State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("Unexcepted message: ~p~n",[Msg]),
	{noreply, State}.

terminate(normal, _) ->
	io:format("Terminating ...~n"),
	exit(test_error).

translate_core(OriginWord) ->
	case OriginWord of
		"casa" -> "house";
		"blanca" -> "white";
		_ -> "I don't understand."
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
