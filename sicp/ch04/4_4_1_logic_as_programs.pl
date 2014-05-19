:- initialization(['common.pl']).
:- initialization(run).

append_to_form([],Y,Y).
append_to_form([U|V],Y,[U|Z]) :-
    append_to_form(V,Y,Z).

% appending [1,2,3] and [4,5,6] to form something
query(findall(X,append_to_form([1,2,3],[4,5,6],X),_)).    

% or we can ask some question the other way around:
query(findall(X,append_to_form([a,b],X,[a,b,c,d]),_)).

% test if it also works on the first argument:
query(findall(X,append_to_form(X,[c,d],[a,b,c,d]),_)).

% and all pairs of lists that append to form [a,b,c,d]
query(findall([X,Y],append_to_form(X,Y,[a,b,c,d]),_)).
