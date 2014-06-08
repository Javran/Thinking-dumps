:- initialization(['common.pl']).
:- initialization(run).

% copied from exercise 4.63.pl:

% son(X,Y): X's son is Y
%         : Y is the son of X
son(adam,cain).
son(cain,enoch).
son(enoch,irad).
son(irad,mehujael).
son(mehujael,methushael).
son(methushael,lamech).
son(ada,jabal).
son(ada,jubal).
son(M,S) :-
    % M's wife is W
    wife(M,W),
    % W's son is S
    son(W,S).
    

% wife(M,W): M's wife is W
wife(lamech,ada).

% grandson: G's grandson is S
grandson(G,S) :-
    % We'd better say:
    % F's son is S
    son(F,S),
    % G's son is F
    son(G,F).

% --- my solution to exercise 4.69:
last_pair([X],[X]).
last_pair([_|T],X) :-
    last_pair(T,X).

% actually I can't see why I need to define this weird rule
% and what's the point of checking if the list ends in word "grandson"
ends_in_grandson(X) :-
    last_pair(X,Y),
    Y = [grandson].

query(findall(X,ends_in_grandson([great,great,grandson]),_)).
query(findall(X,ends_in_grandson([great]),_)).
query(findall(X,ends_in_grandson([grandson]),_)).
