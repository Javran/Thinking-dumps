:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% simple queries
% all computer programmers
query(findall(X, job(X,[computer,programmer]),_)).
% all addresses
query(findall([X,Y1,Y2], address(X,Y1,Y2),_)).
% supervisor of themselves
% query(findall(X, supervisor(X,X)),_).
% no result because there isn't one
% commented out because for now a query of no result seems to halt
% all the following queries
% who has a job that looks like "Computer XXX"
query(findall([X,[computer,T]], job(X,[computer,T]),_)).
% who has a job that begins witn "Computer" (note the difference)
query(findall([X,[computer|T]], job(X,[computer|T]),_)).
