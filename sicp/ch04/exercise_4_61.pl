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

% and also there are two matches in the following list
% if Y = 1:
% - [X,Y] = [2,1]
% - [X,Y] = [3,1]
% therefore we have X = 2 or X = 3
query(findall(X,x_next_to_y_in(X,1,[2,1,3,1]),_)).

% however, if we put X = 1 in the constraint,
% there will be only one match, which is X = 1, Y = 3
query(findall(X,x_next_to_y_in(1,X,[2,1,3,1]),_)).
