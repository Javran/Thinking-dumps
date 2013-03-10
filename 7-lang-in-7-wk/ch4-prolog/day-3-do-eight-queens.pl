% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% Task #3: eight queens
%    this time we will not use list of tuples but list of numbers
%    numbers will be within 1-8
%    we infer queens' col from its value and row from its position in the list

eight_queens(QueenList) :-
	% should be 8 elements
	length(QueenList,8), 
	% each one should be within 1-8
	valid_list(QueenList),

	% verify rows
	% it goes without say that 
	%     the indices of different elements from same list should be different

	% verify cols
	% simply verify if each element from the list is different from each other
	%     will do.
	fd_all_different(QueenList),

	% convert list to list of (row,col) pairs
	list_to_row_col(QueenList,1,RowColList),

	% verify diagonals
	extract_lt_rb(RowColList,LtRbs),
	fd_all_different(LtRbs),

	extract_rt_lb(RowColList,RtLbs),
	fd_all_different(RtLbs),
	format('Solution: ~w~n',[RowColList]).


valid_element(E) :-
	Range = [1,2,3,4,5,6,7,8],
	member(E,Range).

valid_list([]).
valid_list([Head|Tail]) :-
	valid_element(Head),
	valid_list(Tail).


% extract diagonal id from queen list (left-top to right-bottom)
extract_lt_rb([],[]).
extract_lt_rb([(Row,Col)|Tail],[Diag|TailDiagList]) :-
	extract_lt_rb(Tail,TailDiagList),
	Diag is Col - Row.

% extract diagonal id from queen list (right-top to left-bottom)
extract_rt_lb([],[]).
extract_rt_lb([(Row,Col)|Tail],[Diag|TailDiagList]) :-
	extract_rt_lb(Tail,TailDiagList),
	Diag is Col + Row.

	
% assistant: list to (row,col) pair
list_to_row_col([],_,[]).
list_to_row_col(List,Row,TargetList) :-
	% extract col from list
	[Col|Rest] = List,
	RestRow is Row + 1,
	list_to_row_col(Rest,RestRow,TargetRestRow),
	TargetList = [(Row,Col)|TargetRestRow].


% queries
query( findall(X,eight_queens(X),_) ).
