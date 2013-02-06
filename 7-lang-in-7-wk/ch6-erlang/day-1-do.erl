#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	u:ws("Task #1: return word count of a given string"),
	task1(),
	u:ws("Task #2: write a function that count to 10, recursively"),
	task2(),
	u:ws("Task #3: use pattern matching to deal with error/success messages"),
	task3(),
	ok.

task1() ->
	% TODO
	% first we need to know how to split words from a string
	ok.

count_to_and_print(N, N) -> 
	u:wl(N);
count_to_and_print(Current, Limit) ->
	u:wl(Current),
	count_to_and_print(Current+1, Limit).

count_to_10_and_print() ->
	count_to_and_print(1,10).

task2() ->
	count_to_10_and_print().

maybe_success(success) ->
	u:ws("success");

maybe_success({error,Message}) ->
	u:ws("error:"),
	u:ws(Message).

task3() ->
	u:ws("call with 'success':"),
	maybe_success(success),
	u:ws("call with 'error':"),
	maybe_success({error, "This is an error"}),
	maybe_success({error, "Yet another error"}),
	ok.
