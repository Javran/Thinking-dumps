% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

count(0, []).
count(Count, [Head|Tail]) :-
	count(TailCount, Tail),
	Count is TailCount + 1.

sum(0, []).
sum(Sum, [Head|Tail]) :-
	sum(TailSum, Tail),
	Sum is TailSum + Head.

average(Average, List) :-
	sum(Sum, List),
	count(Count, List),
	Average is Sum/Count.

% queries

query( count(What, [1]) ).
query( sum(What, [1,2,3]) ).
query( average(What, [1,2,3,4,5,6]) ).

% dummy query
query_u( '_=_.' ).
