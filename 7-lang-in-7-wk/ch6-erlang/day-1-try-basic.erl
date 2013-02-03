-module(basic).
-export([mirror/1]).

mirror(Anything) -> Anything.

main(_) -> io:format("Result: ~w~n", [mirror(smilling_mug)]).
