:- initialization(['4_4_1_a_sample_database.pl']).
:- initialization(run).

% the name of all people who are supervised by Ben Bitdiddle,
%   together with their addresses
query(findall([P,W1,W2], (supervisor(P,bitdiddleBen),address(P,W1,W2)),_)).

% all people whose salary is less than Ben Bitdiddle's,
%   together with theri salary and Ben Bitdiddle's salary
query(findall([P,PS,BS], (salary(P,PS),salary(bitdiddleBen,BS), PS < BS), _)).

% all people who are supervised by someone who is not
%   in the computer division, together with the supervisor's name and job.
query(findall([P,SN,SJ], (supervisor(P,SN),job(SN,SJ),\+(SJ=[computer|_])), _)). 
