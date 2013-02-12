-module(logger).

-behavior(gen_server).

-export([
	start_link/1,
	post/2,
	stop/1]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

%%% Client API
start_link(LogFileName) -> 
	u:ws("Initializing ..."),
	gen_server:start_link(?MODULE, [LogFileName], []).

% Asynchronous call
post(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

% Synchronous call
stop(Pid) ->
	gen_server:call(Pid, terminate).

%%% Server functions
init([LogFileName]) -> 
	io:format("Opening ~s as log file...~n", [LogFileName]),
	{ok, IoDev} = file:open(LogFileName, [append]),
	{ok, IoDev}.

handle_call(terminate, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(Message, State) ->
	IoDev = State,
	io:format(IoDev, "~s~n", [Message]),
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("Unexcepted message: ~p~n",[Msg]),
	{noreply, State}.

terminate(normal, State) ->
	IoDev = State,
	u:ws("Terminating ..."),
	u:ws("Closing log file..."),
	file:close(IoDev),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
