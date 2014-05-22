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

wife(lamech,ada).

% grandson: G's grandson is S
grandson(G,S) :-
    son(F,S),
    son(G,F).

query(findall(X,grandson(cain,X),_)).
