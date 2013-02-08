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
judge_board(Board) ->
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

print_board(Board) ->
	io:format("+-+-+-+~n"),
	lists:foreach(
		fun(L) ->
			PL = lists:map(
				fun(X) ->
					case X of
						x -> $X;
						o -> $O;
						e -> $\ 
					end
				end,
				L),
			io:format("|~c|~c|~c|~n",PL),
			io:format("+-+-+-+~n")
		end,
		Board).

% wrap print & judgment in one call
judge_tictactoe(Board) ->
	u:ws("The Board is:"),
	print_board(Board),
	u:ws("Judgment is :"),
	Result = judge_board(Board),
	u:wl( Result ),
	Result.

task3() ->
	TestCases = [
		{
			% case #1: x wins
			[ 
				[e,o,x],
				[x,x,o],
				[x,o,e]
			],
			x
		},
		{
			% case #2: o wins
			[ 
				[o,o,o],
				[x,x,o],
				[x,x,e]
			],
			o
		},
		{
			% case #3: no one wins
			[ 
				[o,x,o],
				[o,x,x],
				[x,o,x]
			],
			cat
		},
		{
			% case #4: empty board 
			[ 
				[e,e,e],
				[e,e,e],
				[e,e,e]
			],
			no_winner
		},
		{
			% case #5: playing 
			[ 
				[o,e,o],
				[e,x,e],
				[x,e,e]
			],
			no_winner
		},
		{
			% case #5: impossible 
			[ 
				[o,o,o],
				[x,x,x],
				[e,e,e]
			],
			impossible
		}
	],

	lists:foreach(
		fun({Case,RightAnswer}) ->
			Answer = judge_tictactoe(Case),
			case Answer of
				RightAnswer -> u:ws("The judgement is correct");
				_           -> u:ws("The judgement is incorrect")
			end
		end,
		TestCases).
