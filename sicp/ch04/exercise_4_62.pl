:- initialization(['common.pl']).
:- initialization(run).

% in scheme, the last pair is the last pair in a list / or an improper list
% but here for prolog, I think we need to recognize lists
% with exactly one element as the "last-pair"
last_pair([X],[X]).
last_pair([_|T],X) :-
    last_pair(T,X).

query(findall(X,last_pair([3],X),_)).
query(findall(X,last_pair([1,2,3],X),_)).
query(findall(X,last_pair(2,X),_)).
