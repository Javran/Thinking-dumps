-module(translate_service).
-export([loop/0, translate/2]).

translate_core(OriginWord) ->
	case OriginWord of
		"casa" -> "house";
		"blanca" -> "white";
		_ -> "I don't understand."
	end.

loop() ->
	receive
		{From, stop} ->
			u:ws("<Translator stopping>"),
			From ! stopped;
		{From, OriginWord} ->
			From ! translate_core(OriginWord),
			loop()
	end.

translate(To, Word) ->
	To ! {self(), Word},
	receive
		Translation ->
			u:ws("Translation:"),
			u:ws(Translation)
	end.
