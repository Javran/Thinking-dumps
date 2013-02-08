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

% Board is a list of list, for example: 
% Board =
%     [
%         [x,o,o],
%         [e,x,e],
%         [x,e,o]]
%     where
%         x for player X,
%         o for player O,
%         e for empty
judge_tictactoe(Board) ->
	% define what is line
	Lines = [
		% rows
		[{1,1},{1,2},{1,3}],	
		[{2,1},{2,2},{2,3}],	
		[{3,1},{3,2},{3,3}],	
		% cols
		[{1,1},{2,1},{3,1}],	
		[{1,2},{2,2},{3,2}],	
		[{1,3},{2,3},{3,3}],	
		% diags
		[{1,1},{2,2},{3,3}],	
		[{1,3},{2,2},{3,1}]	
	],
	% if a line is given, turn coords into values(i.e. x/o/e)
	LineToValues =
		fun(Line) ->
			lists:map(
				fun({X,Y}) -> lists:nth(Y, lists:nth(X,Board)) end,
				Line)
		end,

	% if a list of values is given(i.e. [x,x,x]), then returns:
	% if all elements are x -> x
	% if all elements are o -> o
	% if none of the element is e -> full
	% otherwise -> mix
	ValuesToJudgment =
		fun(Values) ->
			case Values of
				[x,x,x] -> x;
				[o,o,o] -> o;
				_ -> 
					IsFull = lists:all(fun(X) -> X /= e end, Values),
					case IsFull of
						true -> full;
						false -> mix
					end
			end
		end,

	% get judgment on all lines
	Results = lists:map(
		fun(L) -> ValuesToJudgment(LineToValues(L)) end,
		Lines),
	XWin = lists:any(fun(X) -> X == x end, Results),
	OWin = lists:any(fun(X) -> X == o end, Results),
	Cat  = lists:all(fun(X) -> X == full end, Results),

	if
		% both x and o owns a line, it's impossible
		XWin and OWin -> impossible;
		XWin -> x;
		OWin -> o;
		Cat -> cat;
		true -> no_winner
	end.


task3() ->
	% TODO pretty print
	u:wl(
	judge_tictactoe([ 
		[o,x,o],
		[x,o,x],
		[x,o,x]])),
	
	ok.
