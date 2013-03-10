% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

male(henry).
male(tom).

married(tom).

bachelor(P) :- male(P), \+ (married(P)).


% queries
query( bachelor(henry) ).
% yes
query( bachelor(tom) ).
% no
query( \+ (married(_))).
% no <- might have been unexpected at first glance
%     but actually we have not told prolog whether Henry or someone else is married
% moreover, this query says:
%     fail if we can find one married people, succeed elsewise
% instead of:
%     find one people who is not married
query( \+ (bachelor(_)) ).
% this query demonstrates the difference between prolog language and what we expected.
% it's wrong to read this query as:
%     find one people who is not bachelor
%          of which the expected answer will be 'tom'
% actually this query reads:
%     fail if we can find one who is not a bachelor, succeed elsewise
