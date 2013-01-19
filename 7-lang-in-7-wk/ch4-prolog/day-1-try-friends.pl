% init
:- initialization(['common.pl']).
:- initialization(run).

% how to run code from file?
% please refer to:
%     http://stackoverflow.com/questions/10059701/simple-prolog-issue-how-do-you-test-multiple-queries-against-your-prolog-databa
% how to print things:
%     http://stackoverflow.com/questions/2556253/prolog-newbie-question-making-a-procedure-to-print-hello-world
% so, 'write' will output things and 'nl' prints a newlin 
% 'halt' will quit the program
% now we can write queries in file and evaluate it, please refer to:
% http://stackoverflow.com/questions/14384917/how-can-i-make-prolog-print-query-results-when-running-a-prolog-script

% facts

likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

% name it 'friend/2'
% if X is not Y and both X & Y like Z then X and Y are friends
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).

% queries

query(likes(wallace, cheese)).
% yes
query(likes(wallace, sheep)).
% no
query(likes(grommit, cheese)).
% yes
query(friend(wallace, wallace)).
% no
query(friend(grommit, wallace)).
% yes because both grommit and wallace like cheese
query(friend(wallace, grommit)).
% yes for the same reason
query(friend(wendolene, grommit)).
% no because they do not share any 'likes'

