#!/usr/bin/env escript

another_factorial(0) -> 1;
another_factorial(N) -> N * another_factorial(N-1).

another_fib(0) -> 1;
another_fib(1) -> 1;
another_fib(N) -> another_fib(N-1) + another_fib(N-2).

% quicker_fib will be obviously faster than another_fib
quicker_fib(0) -> 1;
quicker_fib(1) -> 1;
quicker_fib(T) -> quicker_fib_iter(1,1,T-1).

% use iteration to get result
quicker_fib_iter(_,M,0) -> M;
quicker_fib_iter(N,M,Rest) ->
	quicker_fib_iter(M,M+N,Rest-1).

% check whether the quicker_fib produces correct results in a given range
fib_checker(X, X) -> 
	another_fib(X) == quicker_fib(X);
fib_checker(From, To) ->
	another_fib(From) == quicker_fib(From),
	fib_checker(From+1,To).


main(_) -> 
	io:format("correctness of quicker_fib ... ~w~n",[fib_checker(0,10)]),
	io:format("~w~n", [another_factorial(3)]),
	io:format("~w~n", [another_factorial(20)]),
	io:format("~w~n", [another_factorial(200)]),
	io:format("~w~n", [another_factorial(2000)]),
	% the line below runs VERY SLOW, let's replace it with iteration
	% io:format("~w~n", [another_fib(2000)]),
	io:format("~w~n",[quicker_fib(2000)]),
	io:format("~n",[]).
