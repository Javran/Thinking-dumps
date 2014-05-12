:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% all people supervised by Ben Bitdiddle
query(findall(P, supervisor(P,bitdiddleBen), _)).

% the names and jobs of all people in the accounting division
query(findall([Name,[accounting|Job]], job(Name,[accounting|Job]),_)).

% the names and addresses of all people who live in Slumerville
query(findall([Name,Address], address(Name, slumerville, Address), _)).
