% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

eight_queens(Board) :- 
	% list contains 8 elements
	length(Board,8),
	valid_board(Board),

	extract_rows(Board,Rows),
	fd_all_different(Rows),

	extract_cols(Board,Cols),
	fd_all_different(Cols),

	extract_lt_rb(Board,LtRbs),
	fd_all_different(LtRbs),

	extract_rt_lb(Board,RtLbs),
	fd_all_different(RtLbs).

eight_queens_optimized(Board) :-
	Board = [(1,_),(2,_),(3,_),(4,_),(5,_),(6,_),(7,_),(8,_)],
	valid_board(Board),

	extract_cols(Board,Cols),
	fd_all_different(Cols),

	extract_lt_rb(Board,LtRbs),
	fd_all_different(LtRbs),

	extract_rt_lb(Board,RtLbs),
	fd_all_different(RtLbs).

valid_queen((Row,Col)) :-
	Range = [1,2,3,4,5,6,7,8],
	member(Row,Range),
	member(Col,Range).

valid_board([]).
valid_board([Head|Tail]) :-
	valid_queen(Head),
	valid_board(Tail).

% extract rows from queen list
extract_rows([],[]).
extract_rows([(Row,_)|Tail],[Row|TailRowList]) :-
	extract_rows(Tail,TailRowList).

% extract cols from queen list
extract_cols([],[]).
extract_cols([(_,Col)|Tail],[Col|TailColList]) :-
	extract_cols(Tail,TailColList).

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

% queries

% test: valid_queen
query( valid_queen((1,2)) ).
% yes
query( valid_queen((0,2)) ).
% no

% test: valid_board
query( valid_board([(0,1),(1,2)]) ).
% no
query( valid_board([(2,4),(1,2)]) ).
% yes
query( extract_rows([(1,2),(3,4),(5,6)],_) ).
% [1,3,5]
query( extract_cols([(1,2),(3,4),(5,6)],_) ).
% [2,4,6]
query( extract_lt_rb([(1,1),(2,2),(2,3),(3,4)],_) ).
% [0,0,1,1]
query( extract_rt_lb([(1,8),(2,7),(3,5),(4,4)],_) ).
% [9,9,8,8]

% it will spend roughly 29s on my laptop, uncomment lines below to see the results
% query( findall(
%	[(1,A),(2,B),(3,C),(4,D),(5,E),(6,F),(7,G),(8,H)],
%	eight_queens_optimized([(1,A),(2,B),(3,C),(4,D),(5,E),(6,F),(7,G),(8,H)]),_) ).

% it will spend roughly 4.6s on my laptop, uncomment lines below to see the results
% query_u( 'eight_queens([(1,A),(2,B),(3,C),(4,D),(5,E),(6,F),(7,G),(8,H)]).' ).
% (A,B,C,D,E,F,G,H) = (1,5,8,6,3,7,2,4)

% it will spend roughly 2.3s on my laptop, uncomment lines below to see the results
query_u( 'eight_queens_optimized([(1,A),(2,B),(3,C),(4,D),(5,E),(6,F),(7,G),(8,H)]).' ).
% (A,B,C,D,E,F,G,H) = (1,5,8,6,3,7,2,4)
