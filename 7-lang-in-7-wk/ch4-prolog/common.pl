writeln(T) :- write(T), nl.

% query all tagged 'query' and quit
queryAll :-
	forall(query(Q), (Q ->
		writeln('yes':Q) ;
		writeln('no ':Q))),
	halt.
