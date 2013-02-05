#!/usr/bin/env escript

another_factorial(0) -> 1;
another_factorial(N) -> N * another_factorial(N-1).

another_fib(0) -> 1;
another_fib(1) -> 1;
% I don't think this is the general way of calculating 
% fibonacci sequence given a big sequence number
% TODO: make another_fib work more efficiently
% (attempt to change recursion into iteration, getting rid of repeatly calculation)
another_fib(N) -> another_fib(N-1) + another_fib(N-2).

main(_) -> 
	io:format("~w~n", [another_factorial(3)]),
	io:format("~w~n", [another_factorial(20)]),
	io:format("~w~n", [another_factorial(200)]),
	io:format("~w~n", [another_fib(20)]),
	io:format("~n",[]).
