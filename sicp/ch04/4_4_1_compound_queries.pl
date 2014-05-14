:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% compound queries

% find the addresses of all the computer programmers:
query(findall([P,W1,W2], (job(P,[computer,programmer]),address(P,W1,W2)  ), _)).

% find all employees supervised by Ben Bitdiddle or Alyssa P.Hacker
query(findall(P,(supervisor(P,bitdiddleBen); supervisor(P,hackerAlyssaP)),_)).

% find all people supervised by Ben Bitdiddle
%   who are not computer programmers
query(findall(P,(supervisor(P,bitdiddleBen), \+(job(P,[computer,programmer]))),_)).

% find all people whose salary is greater than $30000
query(findall(P,(salary(P,M),M >= 30000),_)).

% summary:
% "," for conjunction
% ";" for disjunction
% "\+" is negation (actually it's "not provable", and is problematic)
% arithmetic operations are used to do some numeric comparison
