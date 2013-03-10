-module(translator).
-export([
	new/0,	
	translate/2
]).

% translator is responsible for translating words

new() ->
	% keep an orddict as internal state
	orddict:from_list([
		{"casa",
			"house"},
		{"blanca",
			"white"}
	]).

% translate(State, OriginWord) -> {ok, Word} | {error, dontunderstand}}
%     State: returned by new/0
translate(State, OriginWord) ->
	case orddict:find(OriginWord, State) of 
		{ok, Value} ->
			{ok, Value};
		_ ->
			{error, dontunderstand}
	end.
