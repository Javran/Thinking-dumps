% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% Task #1: implement fibonacci sequence and factorial function
% f(0) = 1
factorial(0,1).
factorial(X,F) :- 
	X1 is X - 1,
	factorial(X1,F1),
	F is X * F1.
	
fibonacci(0,1).
fibonacci(1,1).
fibonacci(X,Fib) :-
	X1 is X - 1,
	X2 is X - 2,
	fibonacci(X1, Fib1),
	fibonacci(X2, Fib2),
	Fib is Fib1 + Fib2.

% Task #2: find those communities who use prolog to solve real-world problems
%     what kind of problem can prolog solve?
% 
% TODO
% I cannot find one, the clue might be located at 
%     http://stackoverflow.com/questions/130097/real-world-prolog-usage
% But I've done several searches, and some usage of prolog can be inferred from:
%     http://stackoverflow.com/questions/401635/good-beginners-material-on-prolog/
% seems prolog can be used to deal with problems in
%     * Artifical Intelligence
%     * Natural Language Processing
%     * Discrete Mathematics

% Task #3: solve Tower of Hanoi problem
% hanoi(level, init rod, tmp rod, target rod)

% there is only one disk, so simply moving the disk
%     from A to C will do
hanoi(1,A,_,C) :-
	format('~w -> ~w~n', [A,C]).

hanoi(Level,A,B,C) :-
	PrevLevel is Level - 1,
	hanoi(PrevLevel,A,C,B),
	format('~w -> ~w~n', [A,C]),
	hanoi(PrevLevel,B,A,C).

% Task #4: What are some of the problems of dealing with "not" expressions? 
% Why do you have to be careful with negation in Prolog?
% Some sites that might be useful:
%     http://stackoverflow.com/questions/5472890/prolog-negation
%     http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_5.html
%     http://www.canbaskent.net/logic/early/prolog.pdf
% The simplest answer can be found at the 2nd link
%     please refer to 'day-2-find-negation.pl' 

% queries
query( hanoi(3,a,b,c) ).
% a solution for 7-level hanoi problem has been grabbed 
%     from the output of 'query( hanoi(7,a,b,c) ).'
%     you can find the solution at 'day-2-find-hanoi-7-level-solution.txt'
%     and use the verifier 'day-2-find-hanoi-verifier.py' to verify the solution

% dummy query
query_u( 'factorial( 0,X).' ).
% X=1
query_u( 'factorial(10,X).' ).
% X=3628800 
query_u( 'fibonacci( 1,X).' ).
% X=1
query_u( 'fibonacci(11,X).' ).
% X=144
% TODO: how to make line below work, or is that possible?
% query_u( 'fibonacci(N,144).' ).
