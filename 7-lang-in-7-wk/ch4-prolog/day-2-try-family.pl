% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% fatherOf(X,Y): X is father of Y
fatherOf(zeb,         john_boy_sr).
fatherOf(john_boy_sr, john_boy_jr).

ancestorOf(X, Y) :-
	fatherOf(X, Y).

% if X is Y's father, and Z is Y's ancestor
%     then Z is X's ancestor
ancestorOf(X, Y) :-
	fatherOf(X, Z), ancestorOf(Z, Y).

% queries

query(ancestorOf(john_boy_sr, john_boy_jr)).
% yes
query(ancestorOf(john_boy_jr, john_boy_sr)).
% no
query(ancestorOf(zeb,         john_boy_jr)).
% yes

% zeb is the ancestor of whom?
query(findall(Who, ancestorOf(zeb, Who), _)).

% find all ancestors of john_boy_jr 
query(findall(Who, ancestorOf(Who, john_boy_jr), _)).

% dummy query
query_u( '_=_.' ).
