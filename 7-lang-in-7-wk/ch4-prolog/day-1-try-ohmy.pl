% init
:- initialization(['common.pl']).
:- initialization(run).

% facts
cat(lion).
cat(tiger).

dorothy(X, Y, Z) :- X = lion, Y = tiger, Z = bear.
twin_cats(X, Y) :- cat(X), cat(Y).

query(dorothy(lion, tiger, bear)).
% yes

% comment the line below to suppress warning
% query(dorothy(One, Two, Three)).
% since One, Two, Three are not used here, we can use '_'s
query(dorothy(_, _, _)).
% left side, One unifies with X
% right side, X unifies with lion
% ...

query(findall((One,Two),twin_cats(One, Two),_)).
% list all solutions: lion-lion, lion-tiger ...

% dummy query
query_u( '_=_.' ).
