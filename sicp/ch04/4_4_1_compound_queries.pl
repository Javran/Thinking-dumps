:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% compound queries
addressOfProgrammer(Person,[Where1,Where2]) :-
    % all the following queries should return successfully
    % (or we can say that all conditions need to be satisfied)
    job(Person, [computer,programmer]),
    address(Person, Where1, Where2).

query(findall([P,W], addressOfProgrammer(P,W), _)).
