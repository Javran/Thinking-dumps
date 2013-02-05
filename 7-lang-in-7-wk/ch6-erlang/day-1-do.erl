#!/usr/bin/env escript

main(_) -> 
	io:format("Task #1: return word count of a given string~n"),
	task1(),
	io:format("Task #2: write a function that count to 10, recursively~n"),
	task2(),
	io:format("Task #3: use pattern matching to deal with error/success messages~n"),
	task3(),
	ok.

task1() ->
	% TODO
	% first we need to know how to split words from a string
	ok.

count_to_and_print(N, N) -> 
	io:format("~w~n",[N]);
count_to_and_print(Current, Limit) ->
	io:format("~w~n",[Current]),
	count_to_and_print(Current+1, Limit).

count_to_10_and_print() ->
	count_to_and_print(1,10).

task2() ->
	count_to_10_and_print().

maybe_success(success) ->
	io:format("success~n");

maybe_success({error,Message}) ->
	io:format("error: ~p~n", [Message]).

task3() ->
	io:format("call with 'success':~n"),
	maybe_success(success),
	io:format("call with 'error':~n"),
	maybe_success({error, "This is an error"}),
	maybe_success({error, "Yet another error"}),
	ok.
