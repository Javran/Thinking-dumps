-module(translate_tcp).

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
	start_link/1,
	stop/1
]).

-export([
	handle_client/1
]).

%%% Client API >>>
start_link(Port) -> 
	u:ws("translate_tcp: starting ..."),
	gen_server:start_link(?MODULE, [Port], []).

stop(Pid) ->
	gen_server:call(Pid, stop_server).

%%% Client API <<<

%%% Server functions >>>
% internal state structure:
% { Socket, Port, SupRef }

% call patterns:
%     stop_server
%

init([Port]) -> 
	u:ws("translate_tcp: initializing ..."),
	{ok, Socket} = gen_tcp:listen(Port, [{packet, 0}, {active, false}, {reuseaddr, true}]),
	io:format("translate_tcp: listening on port ~p ...~n", [Port]),
	% start listen and accept links
	gen_server:cast(self(), listen),
	process_flag(trap_exit, true),
	{ok, { Socket, Port, null }}.

terminate(Reason, State) ->
	case Reason of
		normal -> 
			u:ws("translate_tcp: terminating ...");
		shutdown -> 
			u:ws("translate_tcp: terminating ... (req from supervisor)");
		_ ->
			io:format("translate_tcp: crashed because ~p~n", [Reason])
	end,
	{ Socket, _, _ } = State,
	u:ws("translate_tcp: closing socket ..."),
	gen_tcp:close(Socket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_call(stop_server, _From, State) ->
	u:ws("translate_tcp: stopping ..."),
	{stop, normal, ok, State}.

handle_cast(listen, State) ->
	{ Socket, _, _ } = State,
	% waiting for 4s, in case 'stop/1' might fail because of timeout
	case gen_tcp:accept(Socket, 4 * 1000) of
		{error, timeout} ->
			gen_server:cast(self(), listen),
			{noreply, State};
		{error, Reason} ->
			u:ws("error.."),
			u:wl(Reason),
			{stop, Reason, State};
		{ok, ClientSocket} ->
			{ok, IncomingAddr} = inet:peername(ClientSocket),
			io:format(
				"translate_tcp: incomming connection from ~p ...~n", 
				[IncomingAddr]),

			gen_tcp:controlling_process(ClientSocket, spawn(?MODULE, handle_client, [ClientSocket])),
			gen_server:cast(self(), listen),
			{noreply, State}
	end;
handle_cast(crash, State) ->
	u:ws("translate_tcp: simulate a crash"),
	{stop, simulate_crash, State}.

handle_info(Msg, State) ->
	io:format("Unexcepted message: ~p~n",[Msg]),
	{noreply, State}.

%%% Server functions <<<

%%% Service client handler >>>

handle_client(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			Translator = translate_supervisor:get_service(supervisor_inst),
			OriginWord = string:strip(string:strip(Packet, right, $\n), right, $\r),
			Translation = translate_service:translate(Translator, OriginWord),
			Reply = case Translation of
				{ok, Word} ->
					io_lib:format("~s~n",[Word]);	
				_ ->
					io_lib:format("I don't understand.~n",[])
			end,
			gen_tcp:send(Socket, Reply),
			handle_client(Socket);
		{error, closed} ->
			ok;
		{error, Reason} ->
			io:format("translate_tcp: error(reason is ~p) when handling client.",
				[Reason])
	end.

%%% Service client handler <<< 
