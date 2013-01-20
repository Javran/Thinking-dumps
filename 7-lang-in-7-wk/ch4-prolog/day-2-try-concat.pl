% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

concatenate([], List, List).
concatenate([Head|[]], List, [Head|List]).
concatenate([Head1|[Head2|[]]], List, [Head1,Head2|List]).
concatenate([Head1|[Head2|[Head3|[]]]], List, [Head1,Head2,Head3]).
% the rule below will not work
%    which make [a,b] [c,d] to [[a,b],[c,d]]
% concatenate(Head, List, [Head|List]).

concatenate_v2([], List, List).
concatenate_v2([Head|Tail1], List, [Head|Tail2]) :-
	concatenate_v2(Tail1, List, Tail2).

% queries

query( append([oil],[water],[oil,water]) ).
% yes
query( append([tiny],[bubbles], What) ).
% yes: [tiny, bubbles]
query( append([dessert_topping], Who, [dessert_topping, floor_wax]) ).
% yes: [floor_wax]
query( findall( 
	(One,Two), 
	append(One, Two, [apples, oranges, bananas]),
	_) ).

query( concatenate([], [harry], What) ).
% What = harry
query( concatenate([malfoy], [potter], What) ).
% What = [malfoy, potter]
query( concatenate([malfoy, granger], [potter], What) ).
% What = [malfoy, granger, potter]

query( concatenate_v2([1,2], [3], What) ).
% [1,2,3]
% What happened here:
% rule: [Head-A|Tail1-A], List-A, [Head-A|Tail2-A]
%     ==> [1|2], [3], What
%     ==> Head-A=1, Tail1-A=[2], List-A=[3], What=[1|Tail2-A]
%     ==> we need to find 'Tail2-A'
%     rule: Tail1-A, List-A, Tail2-A
%         ==> [2], [3], Tail2-A
%         ==> we need to find 'Tail2-B'
%         rule: [Head-B|Tail1-B], List-B, [Head-B|Tail2-B]
%             ==> [2], [3], Tail2-A
%             ==> [2|[]], [3], [Head-B|Tail2-B]
%             ==> Head-B=2, Tail1-B=[], List-B=[3]
%             ==> we need to find 'Tail2-B'
%             rule: Tail1-B, List-B, Tail2-B
%                 ==> [], [3], [Tail2-B]
%                 ==> we need to find 'Tail2-B'
%                 rule: [], List-C, List-C
%                     ==> [], [3], [3]
%                     ==> List-C=[3]
%                 ==> Tail2-B=[3]
%             ==> Tail2-B=[3]
%         ==> Tail2-B=[3]
%    ==> Tail2A = [Head-B|Tail2-B] = [2|[3]] = [2,3]
%    ==> What = [1|Tail2-A] = [1|[2,3]] = [1,2,3]

% dummy query
query_u( '_=_.' ).
