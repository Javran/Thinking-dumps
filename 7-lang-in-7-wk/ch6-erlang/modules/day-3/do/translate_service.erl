-module(translate_service).
-export([init/0]).

translate_core(OriginWord) ->
	case OriginWord of
		"casa" -> "house";
		"blanca" -> "white";
		_ -> "I don't understand."
	end.

init() ->
	u:ws("<Translator initialized>"),
	loop().

loop() ->
	receive
		stop ->
			u:ws("<Translator stopped>");
		{From, OriginWord} ->
			From ! translate_core(OriginWord),
			loop()
	end.
