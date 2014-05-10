% init
:- initialization(['common.pl']).
:- initialization(run).

% seems like in prolog the rules are not allowed to interleave
% otherwise only the latest definition will be used.

address(bitdiddleBen, slumerville, ridgeRoad, 10).
address(hackerAlyssaP, cambridge, massAve, 78).
address(fectCyD, cambridge, amesStreet, 3).
address(tweakitLemE, boston, bayStateRoad, 22).
address(reasonerLouis, slumerville, pineTreeRoad, 80).
address(warbucksOliver, swellesley, topHeapRoad, nil).
address(scroogeEben, weston, shadyLane, 10).
address(cratchetRobert, allston, nHarvardStreet, 16).
address(aullDeWitt, slumerville, onionSquare, 5).

job(bitdiddleBen, computerWizard).
job(hackerAlyssaP, computerProgrammer).
job(fectCyD, computerProgrammer).
job(tweakitLem, computerTechnician).
job(reasonerLouis, computerProgrammerTrainee).
job(warbucksOliver, administrationBigWheel).
job(scroogeEben, accountingChiefAccountant).
job(cratchetRobert, accountingScrivener).
job(aullDeWitt, administrationSecretary).

salary(bitdiddleBen, 60000).
salary(hackerAlyssaP, 40000).
salary(fectCyD, 35000).
salary(tweakitLemE, 25000).
salary(reasonerLouis, 30000).
salary(warbucksOliver, 150000).
salary(scroogeEben, 75000).
salary(cratchetRobert, 18000).
salary(aullDeWitt, 25000).

supervisor(hackerAlyssaP, bitdiddleBen).
supervisor(fectCyD, bitdiddleBen).
supervisor(tweakitLemE, bitdiddleBen).
supervisor(reasonerLouis, hackerAlyssaP).
supervisor(bitdiddleBen, warbucksOliver).
supervisor(scroogeEben, warbucksOliver).
supervisor(cratchetRobert, scroogeEben).
supervisor(aullDeWitt, swarbucksOliver).

can_do_job(computerWizard, computerProgrammer).
can_do_job(computerWizard, computerTechnician).
can_do_job(computerProgrammer, computerProgrammerTrainee).

% who's address is near Cambridge?
query(findall(A, address(A,cambridge,_,_), _)).
