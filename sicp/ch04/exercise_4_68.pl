:- initialization(['common.pl']).
:- initialization(run).

last_pair([X],[X]).
last_pair([_|T],X) :-
    last_pair(T,X).

init([X],[]).
init([H|T],[H|T2]) :-
    \+(T=[]),
    init(T,T2).

reverse([],[]).
reverse([H|T],Y) :-
    last_pair(Y,[T]),
    init([H|T],Y).

query(findall(X,init([1,2,3,4],X),_)).
query(findall(X,init([1],X),_)).
query(findall(X,reverse([1,2,3,4],X),_)).
query(findall(X,reverse([1],X),_)).

% reverse(X,[1,2,3]) will not work because of the same reason
% that "last_pair" and "init" has.
% suppose we want to query init(X,[1,2,3])
% we will not have enough information to figure out the
% last element of the original list
