% init
:- initialization(['common.pl']).
:- initialization(run).

query( (1,2,3) = (1,2,3) ).
% yes
query( (1,2,3) = (1,2,3,4) ).
% no
query( (1,2,3) = (3,2,1) ).
% no
query( (A,B,C) = (1,2,3) ).

