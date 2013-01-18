% prolog utils

writeln(T) :- write(T), nl.

% TODO
% methods that can write list elements

% query all tagged 'query' and quit
queryAll :-
	forall(query(Q), (Q ->
		writeln('yes':Q) ;
		writeln('no ':Q))).

run :-
	queryAll,
	halt.
