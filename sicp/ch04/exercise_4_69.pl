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

ends_in_grandson(X) :-
    last_pair(X,Y),
    Y = [grandson].

% Irad is the great-grandson of Adam
% Adam's great-grandson is Irad
% the first "argument" is a list,
% which is of form: [grandson], [great,great,...,grandson]
% great([grandson],...) queries for great grandson
% great([great,grandson],...) queries for great-great grandson
great([grandson], X, Y) :-
    son(X,Z),
    grandson(Z,Y).
great([great|Rels],X,Y) :-
    % note that the order of these conditions does matter.
    % thanks to the hint given by:
    % http://d.hatena.ne.jp/torilon/20100227/1267287564
    % X's son is Z
    son(X,Z),
    % Z's <Rels> is Y
    great(Rels,Z,Y),
    % the relationship list should be ended in "grandson"
    ends_in_grandson(Rels).

% Commented out for clearer output
% query(findall(X,ends_in_grandson([great,great,grandson]),_)).
% query(findall(X,ends_in_grandson([great]),_)).
% query(findall(X,ends_in_grandson([grandson]),_)).

query(findall([G,GGS], great([grandson],G,GGS),_)).
query(findall([G,GGS], great([great,great,great,great,grandson],G,GGS),_)).
query(findall(Rels,great(Rels,adam,irad),_)).
