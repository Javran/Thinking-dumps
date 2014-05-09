% init
:- initialization(['common.pl']).
:- initialization(run).

address(bitdiddleBen, slumerville, ridgeRoad, 10).

query(findall(A, address(A,B,C,D), _)).
