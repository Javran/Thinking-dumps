:- initialization(['common.pl']).
:- initialization(run).

append_to_form([],Y,Y).
append_to_form([U|V],Y,[U|Z]) :-
    append_to_form(V,Y,Z).

query(findall(X,append_to_form([1,2,3],[4,5,6],X),_)).    
