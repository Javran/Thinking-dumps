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

% dummy query
query_u( '_=_.' ).
