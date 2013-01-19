% prolog utils

writeln(T) :- write(T), nl.

% TODO
% methods that can write list elements

% query all tagged 'query' and quit
query_all :-
	forall(query(Q), (Q ->
		format('yes:~w~n',[Q]) ;
		format('no :~w~n',[Q]))).

query_all_u :-
	forall(query_u(Q), 
        ( read_term_from_atom(Q, T, [variable_names(L)]),
          ( T -> writeln('yes':L) ; writeln('no ':Q) )
        )).

run :-
	query_all,
	query_all_u,
	halt.
