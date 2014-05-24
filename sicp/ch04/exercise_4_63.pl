:- initialization(['common.pl']).
:- initialization(run).

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

query(findall(X,grandson(cain,X),_)).
query(findall(X,son(lamech,X),_)).
query(findall(X,grandson(methushael,X),_)).
