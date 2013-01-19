% prolog utils

writeln(T) :- write(T), nl.

% TODO
% methods that can write list elements

% query all tagged 'query' and quit
queryAll :-
	forall(query(Q), (Q ->
		format('yes:~w~n',[Q]) ;
		format('no :~w~n',[Q]))).

queryAllU :-
	forall(queryU(Q), 
        ( read_term_from_atom(Q, T, [variable_names(L)]),
          ( T -> writeln('yes':L) ; writeln('no ':Q) )
        )).

errorHandler :- catch(run, X, error_process(X)).

error_process(existence_error) :- write('err:'),nl,errorHandler.

run :-
	queryAll,
	queryAllU,
	halt.
