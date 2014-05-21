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

% the following line will not work.
% and also I don't think this can work neither.
% simply because there are too many ways to find a list
% which has [3] as the last pair.
% and prologd doesn't know how to exhaustively try
% all the lists and print them out
% (and also that this list can be infinite so
% the program might not terminate)
% query(findall(X,last_pair(X,[3]),_)).
