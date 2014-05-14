:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% compound queries

% find the addresses of all the computer programmers:
query(findall([P,W1,W2], (job(P,[computer,programmer]),address(P,W1,W2)  ), _)).
