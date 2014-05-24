:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% P1 can replace P2
can_replace(P1,P2) :-
    ((job(P1,J), job(P2,J));
     (job(P1,J1), job(P2,J2), can_do_job(J1,J2))),
    \+ (P1 = P2).

% query for a full set of "can-replace" without condition
query(findall([P1,P2], can_replace(P1,P2), _)).

% a. all people who can replace Cy D. Fect
query(findall(P, can_replace(P,fectCyD), _)).

% b. all people who can replace someone who is being paid
% more than they are, together with the two salaries.

% I'm not sure about what does it mean by saying
%   "a person is being paid more than they are."
%   but I think this should be the right answer:
%   the result is of form: (<Person1,Salary1,Person2,Salary2>),
%   which means Person1 can replace Person2,
%   and Person2 is being paid more than Person1.
query(findall([P1,S1,P2,S2],
              (can_replace(P1,P2),
               salary(P1,S1),
               salary(P2,S2),
               S1<S2),
              _)).
