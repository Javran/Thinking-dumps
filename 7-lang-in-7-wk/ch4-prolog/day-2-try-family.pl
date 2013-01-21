% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% father_of(X,Y): X is father of Y
father_of(zeb,         john_boy_sr).
father_of(john_boy_sr, john_boy_jr).

ancestor_of(X, Y) :-
	father_of(X, Y).

% if X is Y's father, and Z is Y's ancestor
%     then Z is X's ancestor
ancestor_of(X, Y) :-
	father_of(X, Z), ancestor_of(Z, Y).

% queries

query(ancestor_of(john_boy_sr, john_boy_jr)).
% yes
query(ancestor_of(john_boy_jr, john_boy_sr)).
% no
query(ancestor_of(zeb,         john_boy_jr)).
% yes

% zeb is the ancestor of whom?
query(findall(Who, ancestor_of(zeb, Who), _)).

% find all ancestors of john_boy_jr 
query(findall(Who, ancestor_of(Who, john_boy_jr), _)).

% dummy query
query_u( '_=_.' ).
