% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% Task #1: modify sudoku solver to solve 6x6 sudokus

% TODO: I think the method used here is only a quick-and-dirty way
%     I want a more decent solution

sudoku_6x6(Puzzle, Solution) :-
	Puzzle = Solution,
	% unification to prevent invalid puzzle
	Puzzle = [
		S11,S12,S13,S14,S15,S16,
		S21,S22,S23,S24,S25,S26,
		S31,S32,S33,S34,S35,S36,
		S41,S42,S43,S44,S45,S46,
		S51,S52,S53,S54,S55,S56,
		S61,S62,S63,S64,S65,S66],

	% limit: number must be within [1,6]
	fd_domain(Puzzle,1,6),
	% define rows 
	Row1 = [S11,S12,S13,S14,S15,S16],
	Row2 = [S21,S22,S23,S24,S25,S26],
	Row3 = [S31,S32,S33,S34,S35,S36],
	Row4 = [S41,S42,S43,S44,S45,S46],
	Row5 = [S51,S52,S53,S54,S55,S56],
	Row6 = [S61,S62,S63,S64,S65,S66],

	% define cols
	Col1 = [S11,S21,S31,S41,S51,S61],
	Col2 = [S12,S22,S32,S42,S52,S62],
	Col3 = [S13,S23,S33,S43,S53,S63],
	Col4 = [S14,S24,S34,S44,S54,S64],
	Col5 = [S15,S25,S35,S45,S55,S65],
	Col6 = [S16,S26,S36,S46,S56,S66],

	% define boxes
	% +---+---+
	% | 1 | 2 |
	% +---+---+
	% | 3 | 4 |
	% +---+---+
	% | 5 | 6 |
	% +---+---+
	Box1 = [S11,S12,S13,S21,S22,S23],
	Box2 = [S14,S15,S16,S24,S25,S26],
	Box3 = [S31,S32,S33,S41,S42,S43],
	Box4 = [S34,S35,S36,S44,S45,S46],
	Box5 = [S51,S52,S53,S61,S62,S63],
	Box6 = [S54,S55,S56,S64,S65,S66],

	% verify puzzle
	valid([
		Row1,Row2,Row3,Row4,Row5,Row6,
		Col1,Col2,Col3,Col4,Col5,Col6,
		Box1,Box2,Box3,Box4,Box5,Box6]).

sudoku_9x9(Puzzle,Solution) :-
	Puzzle = Solution,
	Puzzle = [
		S11,S12,S13,S14,S15,S16,S17,S18,S19,
		S21,S22,S23,S24,S25,S26,S27,S28,S29,
		S31,S32,S33,S34,S35,S36,S37,S38,S39,
		S41,S42,S43,S44,S45,S46,S47,S48,S49,
		S51,S52,S53,S54,S55,S56,S57,S58,S59,
		S61,S62,S63,S64,S65,S66,S67,S68,S69,
		S71,S72,S73,S74,S75,S76,S77,S78,S79,
		S81,S82,S83,S84,S85,S86,S87,S88,S89,
		S91,S92,S93,S94,S95,S96,S97,S98,S99
	],
	fd_domain(Puzzle,1,9),
	% define rows
	Row1 = [S11,S12,S13,S14,S15,S16,S17,S18,S19],
	Row2 = [S21,S22,S23,S24,S25,S26,S27,S28,S29],
	Row3 = [S31,S32,S33,S34,S35,S36,S37,S38,S39],
	Row4 = [S41,S42,S43,S44,S45,S46,S47,S48,S49],
	Row5 = [S51,S52,S53,S54,S55,S56,S57,S58,S59],
	Row6 = [S61,S62,S63,S64,S65,S66,S67,S68,S69],
	Row7 = [S71,S72,S73,S74,S75,S76,S77,S78,S79],
	Row8 = [S81,S82,S83,S84,S85,S86,S87,S88,S89],
	Row9 = [S91,S92,S93,S94,S95,S96,S97,S98,S99],

	% define cols
	Col1 = [S11,S21,S31,S41,S51,S61,S71,S81,S91],
	Col2 = [S12,S22,S32,S42,S52,S62,S72,S82,S92],
	Col3 = [S13,S23,S33,S43,S53,S63,S73,S83,S93],
	Col4 = [S14,S24,S34,S44,S54,S64,S74,S84,S94],
	Col5 = [S15,S25,S35,S45,S55,S65,S75,S85,S95],
	Col6 = [S16,S26,S36,S46,S56,S66,S76,S86,S96],
	Col7 = [S17,S27,S37,S47,S57,S67,S77,S87,S97],
	Col8 = [S18,S28,S38,S48,S58,S68,S78,S88,S98],
	Col9 = [S19,S29,S39,S49,S59,S69,S79,S89,S99],

	% define boxes
	Box1 = [S11,S12,S13, S21,S22,S23, S31,S32,S33],
	Box2 = [S14,S15,S16, S24,S25,S26, S34,S35,S36],
	Box3 = [S17,S18,S19, S27,S28,S29, S37,S38,S39],

	Box4 = [S41,S42,S43, S51,S52,S53, S61,S62,S63],
	Box5 = [S44,S45,S46, S54,S55,S56, S64,S65,S66],
	Box6 = [S47,S48,S49, S57,S58,S59, S67,S68,S69],
	
	Box7 = [S71,S72,S73, S81,S82,S83, S91,S92,S93],
	Box8 = [S74,S75,S76, S84,S85,S86, S94,S95,S96],
	Box9 = [S77,S78,S79, S87,S88,S89, S97,S98,S99],

	% verify puzzle
	valid([
		Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,Row9,
		Col1,Col2,Col3,Col4,Col5,Col6,Col7,Col8,Col9,
		Box1,Box2,Box3,Box4,Box5,Box6,Box7,Box8,Box9
	]).

% judge if all groups are valid
valid([]).
valid([Head|Tail]) :-
	valid(Tail),
	fd_all_different(Head).

% queries
query( sudoku_6x6([
	3,_,_,1,_,_,
	_,_,1,_,5,3,
	_,2,_,_,_,_,
	_,_,5,4,_,6,
	5,1,_,3,_,2,
	_,_,3,_,_,_],_) ).
% yes:
% 3 5 2 1 6 4
% 4 6 1 2 5 3
% 6 2 4 5 3 1
% 1 3 5 4 2 5
% 5 1 6 3 4 2
% 2 4 3 6 1 5

query( sudoku_9x9([
	3,_,_, _,_,1, _,8,_,
	_,_,_, 4,3,_, _,_,1,
	_,9,_, _,_,8, 2,_,_,
	_,_,6, _,9,_, 1,4,_,
	_,_,_, _,_,5, _,_,_,
	_,7,1, _,_,_, _,9,_,
	6,_,_, 8,_,_, 3,_,7,
	_,_,_, 9,_,_, 5,_,_,
	7,_,3, _,2,_, _,_,_
	],_) ).
% yes:
% 3 4 7 2 5 1 9 8 6
% 2 6 8 4 3 9 7 5 1
% 1 9 5 7 6 8 2 3 4
% 8 2 6 3 9 7 1 4 5
% 9 3 4 1 8 5 6 7 2
% 5 7 1 6 4 2 8 9 3
% 6 5 9 8 1 4 3 2 7
% 4 1 2 9 7 3 5 6 8
% 7 8 3 5 2 6 4 1 9

% dummy query
query_u( '_=_.' ).
