% how to run code from file?
% please refer to:
%     http://stackoverflow.com/questions/10059701/simple-prolog-issue-how-do-you-test-multiple-queries-against-your-prolog-databa
% how to print things:
%     http://stackoverflow.com/questions/2556253/prolog-newbie-question-making-a-procedure-to-print-hello-world
% so, 'write' will output things and 'nl' prints a newlin 
% 'halt' will quit the program
% an useful link about some prolog tutorials:
%     http://stackoverflow.com/questions/401635/good-beginners-material-on-prolog

% currently 'write' print the argument as if it is a string
%    (but strangely removed white space between "," and "c")
% how make it work here?
main :-
	write(likes(wallace, cheese)),
	halt.

:- initialization(['day-1-try-friends-facts']).
:- initialization(main).
