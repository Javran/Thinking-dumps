:- initialization(['common.pl']).
:- initialization(run).

x_next_to_y_in(X,Y,[X,Y|_]).
x_next_to_y_in(X,Y,[V|Z]) :-
    x_next_to_y_in(X,Y,Z).

% from rule we can see that X and Y are organized in order,
% which means if "x" next to "y" in "z" holds,
% "y" next to "x" in "z" will not.
% then the answers will be:
% - 1,[2,3]
% - [2,3],4
query(findall([X,Y], x_next_to_y_in(X,Y,[1,[2,3],4]),_)).


query(findall(X,x_next_to_y_in(X,1,[2,1,3,1]),_)).
