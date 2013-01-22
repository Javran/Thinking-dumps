% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

eight_queens(List) :- 
	% list contains 8 elements
	length(List,8).

valid_queen((Row,Col)) :-
	Range = [1,2,3,4,5,6,7,8],
	member(Row,Range),
	member(Col,Range).

% queries
query( true ).

% dummy query
query_u( '_=_.' ).
