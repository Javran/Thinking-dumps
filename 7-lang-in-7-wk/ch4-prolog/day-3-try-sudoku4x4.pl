% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

sudoku(Puzzle, Solution) :-
	Puzzle = Solution,
	% unification to prevent invalid puzzle
	Puzzle = [
		S11,S12,S13,S14,
		S21,S22,S23,S24,
		S31,S32,S33,S34,
		S41,S42,S43,S44],
	% limit: number must be within [1,4]
	fd_domain(Puzzle,1,4),
	% define rows 
	Row1 = [S11,S12,S13,S14],
	Row2 = [S21,S22,S23,S24],
	Row3 = [S31,S32,S33,S34],
	Row4 = [S41,S42,S43,S44],
	% define cols
	Col1 = [S11,S21,S31,S41],
	Col2 = [S12,S22,S32,S42],
	Col3 = [S13,S23,S33,S43],
	Col4 = [S14,S24,S34,S44],
	% define boxes
	Box1 = [S11,S12,S21,S22],
	Box2 = [S13,S14,S23,S24],
	Box3 = [S31,S32,S41,S42],
	Box4 = [S33,S34,S43,S44],
	% verify puzzle
	valid([
		Row1,Row2,Row3,Row4,
		Col1,Col2,Col3,Col4,
		Box1,Box2,Box3,Box4]).

% judge if all groups are valid
valid([]).
valid([Head|Tail]) :-
	valid(Tail),
	fd_all_different(Head).

% queries

% solved sudoku:
query( sudoku([
	4,1,2,3,
	2,3,4,1,
	1,2,3,4,
	3,4,1,2],_) ).
% yes

% illegal sudokus:
query( sudoku([1,2,3],_) ).
% no
query( sudoku([
	1,2,3,4,
	5,6,7,8,
	9,0,1,2,
	3,4,5,6],_) ).
% no
query( sudoku([
	_,_,2,3,
	_,_,_,_,
	_,_,_,_,
	3,4,_,_],_) ).
% yes:
% 4 1 2 3
% 2 3 4 1
% 1 2 3 4
% 3 4 1 2

% dummy query
query_u( '_=_.' ).
