% prolog utils

% you might encounter some exception if 'query' or 'query_u' are not defined,
%     to solve this, you can either:
%         use 'dynamic' (by uncomment lines like `:- dynamic ...` below) or 
%         use error handling in prolog, of which description can be found at 
%     http://www.gprolog.org/manual/gprolog.html#htoc68  (I'm using gprolog)
% please refer to:
%     http://stackoverflow.com/questions/14522212/error-handling-in-gprolog/

% do all queries marked 'query'
%:- dynamic(query/1).
do_all_query :-
	format('>>> do all ''query''s~n',[]),
	catch(
		(
			forall(query(Q),
				(Q ->
					format('yes:~w~n',[Q]) ;
					format('no :~w~n',[Q]))),
			format('>>> all ''query''s are done~n',[])
		),
		error(existence_error(procedure,_),_),
		format('<<< no ''query'' founded.~n',[])).

% do all queries marked 'query_u' and show unification results
% :- dynamic(query_u/1).
do_all_query_u :-
	format('>>> do all ''query_u''s~n',[]),
	catch(
		(
			forall(query_u(Q), 
        			(
					read_term_from_atom(Q, T, [variable_names(L)]),
          				(T -> 
						format('yes:~w~n~w~n',[Q,L]) ; 
						format('no :~w~n'    ,[Q  ]) ))),
			format('>>> all ''query_u''s are done~n',[])
		),
		error(existence_error(procedure,_),_),
		format('<<< no ''query_u'' founded.~n',[])).

run :-
	do_all_query,
	do_all_query_u,
	halt.
