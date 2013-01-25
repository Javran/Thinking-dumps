% init
:- initialization(['common.pl']).
:- initialization(run).

% to see unification results, please refer to:
% http://stackoverflow.com/questions/14416099/how-can-i-print-unification-results-when-using-prolog-script

% test unification on tuples

query_u( '(1,2,3) = (1,2,3).' ).
% yes
query_u( '(1,2,3) = (1,2,3,4).' ).
% no
query_u( '(1,2,3) = (3,2,1).' ).
% no
query_u( '(A,B,C) = (1,2,3).' ).
% yes
query_u( '(1,2,3) = (A,B,C).' ).
% yes
query_u( '(1,B,3,D) = (A,2,C,4).' ).
% yes

% test unification on lists
query_u( '[1,2,3] = [1,2,3].' ).
% yes
query_u( '[1,2,3] = [X,Y,Z].' ).
% yes
query_u( '[2,2,3] = [X,X,Z].' ).
% yes
query_u( '[1,2,3] = [X,X,Z].' ).
% no

query_u( '[a,b,c] = [Head|Tail].' ).
% yes
query_u( '[] = [Head|Tail].' ). 
% no
query_u( '[a] = [Head|Tail].' ).
% yes
query_u( '[a,b,c] = [a|Tail].' ).
% yes
% [b,c] = Tail
query_u( '[a,b,c] = [a|[Head|Tail]].' ).
% yes
% [b,c] = [Head|Tail]
query_u( '[a,b,c,d,e] = [_,_|[Head|_]].' ).
% yes
% [c,d,e] = [Head|_]
% [c] = [Head]
