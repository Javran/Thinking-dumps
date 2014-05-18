:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(['4_4_1_rules.pl']).
:- initialization(run).

% query all possible combinations
query(findall([P1,P2], lives_near(P1,P2), _)).
query(findall(P, wheel(P), _)).
query(findall([S,B], outranked_by(S,B), _)).

% query just for some cases
query(findall(X, lives_near(X,bitdiddleBen), _)).
query(findall(P, (job(P,[computer,programmer]), lives_near(P,bitdiddleBen)), _)).
