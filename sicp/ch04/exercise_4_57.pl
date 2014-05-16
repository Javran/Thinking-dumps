:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% P1 can replace P2
can_replace(P1,P2) :-
    ((job(P1,J), job(P2,J));
     (job(P1,J1), job(P2,J2), can_do_job(J1,J2))),
    \+ (P1 = P2).

% query for a full set of "can-replace" without condition
query(findall([P1,P2], can_replace(P1,P2), _)).
