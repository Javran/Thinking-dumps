% facts

likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

% name it 'friend/2'
% if X is not Y and both X & Y like Z then X and Y are friends
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).

% queries

query(likes(wallace, cheese)).
% yes
query(likes(wallace, sheep)).
% no
query(likes(grommit, cheese)).
% yes
query(friend(wallace, wallace)).
% no
query(friend(grommit, wallace)).
% yes because both grommit and wallace like cheese
query(friend(wallace, grommit)).
% yes for the same reason
query(friend(wendolene, grommit)).
% no because they do not share any 'likes'

:- initialization(['common.pl']).
:- initialization(queryAll).
