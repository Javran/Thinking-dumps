% prolog utils

writeln(T) :- write(T), nl.

% TODO
% methods that can write list elements

% do all queries marked 'query'
do_all_query :-
	forall(query(Q),
		(Q ->
			format('yes:~w~n',[Q]) ;
			format('no :~w~n',[Q]))).

% do all queries marked 'query_u' and show unification results
do_all_query_u :-
	forall(query_u(Q), 
        	(
			read_term_from_atom(Q, T, [variable_names(L)]),
          		(T -> 
				format('yes:~w~n~w~n',[Q,L]) ; 
				format('no :~w~n'    ,[Q  ]) ))).

run :-
	do_all_query,
	do_all_query_u,
	halt.
