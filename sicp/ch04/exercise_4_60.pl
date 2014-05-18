:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(['4_4_1_rules.pl']).
:- initialization(run).

list_less_than(A,B) :-
    length(A,LA),
    length(B,LB),
    LA < LB.
list_less_than(A,B) :-
    length(A,LA),
    length(B,LB),
    LA=LB,
    \+(A=B),
    [HA|TA]=A,
    [HB|TB]=B,
    (HA < HB;
     list_less_than(TA,TB)).

atom_compare(A,B) :-
    atom_codes(A,CA),
    atom_codes(B,CB),
    list_less_than(CA,CB).

% the results are doubled in the following query,
%   simply because if P1 is near P2, then P2 much be near P1
%   in order to limit the result, we might need to put some order on people
query(findall([P1,P2], lives_near(P1,P2), _)).

% the duplications are eliminated in the following query.
query(findall([P1,P2], (lives_near(P1,P2),atom_compare(P1,P2)), _)).
