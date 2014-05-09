% init
:- initialization(['common.pl']).
:- initialization(run).

% seems like in prolog the rules are not allowed to interleave
% otherwise only the latest definition will be used.

address(bitdiddleBen, slumerville, ridgeRoad, 10).
address(hackerAlyssaP, cambridge, massAve, 78).
address(fectCyD, cambridge, amesStreet, 3).


job(bitdiddleBen, computerWizard).
job(hackerAlyssaP, computerProgrammer).
job(fectCyD, computerProgrammer).

salary(bitdiddleBen, 60000).
salary(hackerAlyssaP, 40000).
salary(fectCyD, 35000).

supervisor(hackerAlyssaP, bitdiddleBen).
supervisor(fectCyD, bitdiddleBen).

% who's address is near Cambridge?
query(findall(A, address(A,cambridge,_,_), _)).
