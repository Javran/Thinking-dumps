% init
:- initialization(['common.pl']).

% seems like in prolog the rules are not allowed to interleave
% otherwise only the latest definition will be used.

address(bitdiddleBen, slumerville, [ridge,road,10]).
address(hackerAlyssaP, cambridge, [mass,ave,78]).
address(fectCyD, cambridge, [ames,street,3]).
address(tweakitLemE, boston, [bay,state,road,22]).
address(reasonerLouis, slumerville, [pineTree, road, 80]).
address(warbucksOliver, swellesley, [topHeap,road,nil]).
address(scroogeEben, weston, [shady,lane,10]).
address(cratchetRobert, allston, [nHarvard,street,16]).
address(aullDeWitt, slumerville, [onion,square,5]).

job(bitdiddleBen, [computer,wizard]).
job(hackerAlyssaP, [computer,programmer]).
job(fectCyD, [computer,programmer]).
job(tweakitLem, [computer,technician]).
job(reasonerLouis, [computer,programmer,trainee]).
job(warbucksOliver, [administration,big,wheel]).
job(scroogeEben, [accounting,chief,accountant]).
job(cratchetRobert, [accounting,scrivener]).
job(aullDeWitt, [administration,secretary]).

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
supervisor(aullDeWitt, warbucksOliver).

can_do_job([computer,wizard], [computer,programmer]).
can_do_job([computer,wizard], [computer,technician]).
can_do_job([computer,programmer], [computer,programmer,trainee]).
can_do_job([administration,secretary], [administration,big,wheel]).
