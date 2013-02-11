-module(roulette).
-export([loop/0]).

% send a number, 1-6
loop() ->
	receive
		3 -> 
			u:ws("bang."),
			exit({roulette,die,at,erlang:time()});
		_ ->
			u:ws("click."),
			loop()
	end.
