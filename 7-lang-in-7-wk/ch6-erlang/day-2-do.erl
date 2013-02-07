#!/usr/bin/env escript



main(_) -> 
	c:c(u),

	u:ws("Task #1: write a dict, where when key is given, returns the value"),
	task1(),

	u:ws("Task #2: {item, quantity, price} => {item, total_price}"),
	task2(),

	u:ws("Task #3: Tic-tac-toe judgment"),	
	task3(),
	ok.

find_in_dict(Dict, Key) ->
	GetKey = 
		fun({K,_}) -> K end,
	GetValue = 
		fun({_,V}) -> V end,
	Result = lists:filter(
		fun(KeyValue) -> GetKey(KeyValue) == Key end,
		Dict),
	% if nothing is found, would fail when reach next line
	[H|_] = Result,
	GetValue(H).	

task1() ->
	u:ws("Dict:"),
	Dict = [{erlang, "a functional language"}, {ruby, "an OO language"}],
	lists:map(fun({K,V}) -> io:format("{~w,~p}~n",[K,V]) end, Dict),
	lists:map(
		fun(K) ->
			io:format("Find value of '~w':~n",[K]),
			u:ws(find_in_dict(Dict, K))
		end,
		[erlang, ruby]),
	ok.

task2() ->
	u:ws("ShoppingList:"),
	ShoppingList = [
		{pencil, 4, 0.25},
		{pen, 1, 1.20},
		{paper, 2, 0.20}],
	u:wl(ShoppingList),

	u:ws("Target:"),
	Target = [{Item, Quantity*Price} || {Item, Quantity, Price} <- ShoppingList],
	u:wl(Target),

	u:ws("TotalPrice:"),
	% 2. foldl to get the sum
	u:wl(lists:foldl(
		fun(I, Acc) -> Acc+I end,
		0,
		% 1. tuple to price
		lists:map(fun({_,T}) -> T end, Target))),
	ok.

task3() ->
	% TODO
	ok.
