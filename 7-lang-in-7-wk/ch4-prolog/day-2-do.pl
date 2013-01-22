% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% Task #1: reverse a list
reverse_list([], []).
reverse_list([Head|Tail], ReversedList) :-
	% first we attempt to reverse the tail
	reverse_list(Tail, ReversedTailList),
	append(ReversedTailList, [Head], ReversedList).

% Task #2: find the minimial element in the list
% min_from two: pick the minimal one from two
min_from_two(A,B,Min) :-
	A =< B,
	Min = A.

min_from_two(A,B,Min) :-
	A > B,
	Min = B.

min_in_list([X],X).
min_in_list([Head|Tail], Min) :-
	% find the min in tail
	min_in_list(Tail, TailMin),
	min_from_two(Head, TailMin, Min).

% Task #3: sort a list
% only keep elements that =< X in List
keep_less_equal(X,[],[]).

keep_less_equal(X,[Head|Tail],TargetList) :-
	Head =< X,
	keep_less_equal(X,Tail,TailTargetList),
	append([Head],TailTargetList,TargetList).

keep_less_equal(X,[Head|Tail],TargetList) :-
	Head > X,
	keep_less_equal(X,Tail,TargetList).

% only keep elements that > X in List
keep_greater(X,[],[]).

keep_greater(X,[Head|Tail],TargetList) :-
	Head > X,
	keep_greater(X,Tail,TailTargetList),
	append([Head],TailTargetList,TargetList).

keep_greater(X,[Head|Tail],TargetList) :-
	Head =< X,
	keep_greater(X,Tail,TargetList).

quick_sort([],[]).
quick_sort([Head|Tail],Sorted) :-
	keep_less_equal(Head,Tail,LessEqualList),
	keep_greater(Head,Tail,GreaterList),
	quick_sort(LessEqualList,LESorted),
	quick_sort(GreaterList,GSorted),
	append(LESorted,[Head],Sorted1),
	append(Sorted1,GSorted,Sorted).

% TODO: find if it is possible to use some equivalent to 'if' statements
%    which will obviously shorten the code

% queries

query( reverse_list([a,b,c,1,2,3,4,5], _) ).
query( min_in_list([5,6,7,4,3,8,9], _) ).
query( keep_less_equal(5,[1,2,3,4,5,1,2,3,7,8,9,7,8,9],_) ).
query( keep_greater(5,[1,2,3,4,5,1,2,3,7,8,9,7,8,9],_) ).
query( quick_sort([1,2,3,4,5,1,2,3,7,8,9,7,8,9],_) ).

% dummy query
query_u( '_=_.' ).
