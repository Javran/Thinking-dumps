% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% Task #1: find predication that can read/write variables
read_and_write :-
	% use '.' to terminate your input
	read(X),
	format('user input: ~w~n',[X]).

% Task #2: print things only when query succeeds
print_if_succeed(X) :-
	X,
	format('query ok: ~w~n',[X]).

is_ok(ok).

% queries
query( read_and_write ).
query( print_if_succeed(is_ok(ok)) ).
query( print_if_succeed(is_ok(no)) ).

% dummy query
query( true ).

% dummy query
query_u( '_=_.' ).
