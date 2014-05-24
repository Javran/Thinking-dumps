:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(['4_4_1_rules.pl']).
:- initialization(run).

% compare two lists and determine
%   if the first one is less than the second one
list_less_than(A,B) :-
    length(A,LA),
    length(B,LB),
    % if the length is not equal, we can simply compare the length
    LA < LB.
list_less_than(A,B) :-
    length(A,LA),
    length(B,LB),
    % if the length are equal, two list can't be exactly the same
    %   if the inequality holds.
    % a consequence follows immediately that A and B cannot be empty
    LA=LB,
    \+(A=B),
    % therefore we pattern match the list, compare their head elements
    %   if there is an equality in the head, that's it.
    %   or otherwise, we clamin recursively that the tail of the first one
    %   shall be less than the tail of the second one
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
