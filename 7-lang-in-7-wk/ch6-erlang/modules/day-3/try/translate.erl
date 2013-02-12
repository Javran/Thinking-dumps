-module(translate).
-export([loop/0]).

loop() ->
	receive
		"casa" ->
			u:ws("house"),
			loop();
		"blanca" ->
			u:ws("white"),
			loop();
		":stop" ->
			u:ws("<Stop process>");
			% loop/0 is used to keep process going
			%     we remove `loop()` here, 
			%     so the process will stop
		_ ->
			u:ws("I don't understand."),
			loop()
	end.
