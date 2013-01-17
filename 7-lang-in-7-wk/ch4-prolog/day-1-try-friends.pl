% prolog query

% how to run code from file?
% please refer to:
%     http://stackoverflow.com/questions/10059701/simple-prolog-issue-how-do-you-test-multiple-queries-against-your-prolog-databa
% how to print things:
%     http://stackoverflow.com/questions/2556253/prolog-newbie-question-making-a-procedure-to-print-hello-world
% so, 'write' will output things and 'nl' prints a newlin 
% 'halt' will quit the program


% TODO: why prints 'q1' while the result expected? 
test :-
	write(q1),
	nl.

:- initialization(['day-1-try-friends-db']).
:- initialization(test).
