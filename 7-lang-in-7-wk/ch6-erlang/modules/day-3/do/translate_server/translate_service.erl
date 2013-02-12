-module(translate_service).

-behavior(gen_server).

-export([
	init/1,
	terminate/2,
	code_change/3,

	handle_call/3,
	handle_cast/2,
	handle_info/2
]).

-export([
	start_link/0,
	stop/1,

	translate/2,
	crash/1
]).

%%% Client API >>>
start_link() -> 
	u:ws("translate_service: starting ..."),
	gen_server:start_link(?MODULE, [], []).

%% Synchronous call
translate(Pid, OriginWord) ->
	gen_server:call(Pid, {translate, OriginWord}).

%% Synchronous call
stop(Pid) ->
	gen_server:call(Pid, stop_server).

%% Asynchronous call
% call crash/1 to simulate a crash on the process 
crash(Pid) ->
	gen_server:cast(Pid, crash).
%%% Client API <<<

%%% Server functions >>>
% internal state structure:
% { TranslatorState }

% call patterns:
%     {translate, OriginWord}
%     stop_server
%
% cast patterns:
%     crash
%

init([]) -> 
	u:ws("translate_service: initializing ..."),
	TranslatorState = translator:new(),
	process_flag(trap_exit, true),
	{ok, {TranslatorState}}.

terminate(normal, _) ->
	u:ws("translate_service: terminating ..."),
	ok;
terminate(shutdown, _) ->
	u:ws("translate_service: terminating ... (req from supervisor)"),
	ok;
terminate(Reason, _) ->
	io:format("translate_service: crashed because ~p~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_call({translate, OriginWord}, _From, State) ->
	{TranslatorState} = State,
	{reply, translator:translate(TranslatorState, OriginWord), State};
handle_call(stop_server, _From, State) ->
	u:ws("translate_service: stopping ..."),
	{stop, normal, ok, State}.

handle_cast(crash, State) ->
	u:ws("translate_service: simulate a crash"),
	{stop, simulate_crash, State}.

handle_info(Msg, State) ->
	io:format("Unexcepted message: ~p~n",[Msg]),
	{noreply, State}.

%%% Server functions <<<
