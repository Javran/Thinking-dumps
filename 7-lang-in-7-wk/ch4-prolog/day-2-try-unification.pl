% init
:- initialization(['common.pl']).
:- initialization(run).

% dummy query
query( true ).

% to see unification results, please refer to:
% http://stackoverflow.com/questions/14416099/how-can-i-print-unification-results-when-using-prolog-script

query_u( '(1,2,3) = (1,2,3).' ).
% yes
query_u( '(1,2,3) = (1,2,3,4).' ).
% no
query_u( '(1,2,3) = (3,2,1).' ).
% no
query_u( '(A,B,C) = (1,2,3).' ).

