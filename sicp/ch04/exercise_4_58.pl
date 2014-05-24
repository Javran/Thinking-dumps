:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% a "big shot" in a division
%   is a person works in the division
%   but does not have a supervisor who works
%   in the division
big_shot(P) :-
    supervisor(P,SV),
    job(P,[DivP|_]),
    job(SV,[DivSV|_]),
    \+(DivP = DivSV).

query(findall(P, big_shot(P), _)).
