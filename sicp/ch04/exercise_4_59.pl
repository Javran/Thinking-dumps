:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

meeting(accounting, monday, 9, am).
meeting(administration, monday, 10, am).
meeting(computer, wednesday, 3, pm).
meeting(administration, friday, 1, pm).
meeting(whole_company, wednesday, 4, pm).

meeting_time(P,Day,T1,T2) :-
    job(P,[Div|_]),
    (meeting(whole_company,Day,T1,T2);
     meeting(Div,Day,T1,T2)).

% a. all the meetings that occur on Friday
query(findall([Div,T1,T2], meeting(Div,friday,T1,T2), _)).

% b. query the meeting for a given person and time
%   here I also make a sample query: Cy D. Fect's meeting on Wednesday.
query(findall([T1,T2], meeting_time(fectCyD,wednesday,T1,T2), _)). 

% c. all of the meetings on Wednesday that Alyssa has to attend:
query(findall([T1,T2], meeting_time(hackerAlyssaP,wednesday,T1,T2), _)).
