#!/usr/bin/env scala
!#

// Task #1: Tic-Tac-Toe Game
class TicTacToe {

	// judge whether a board is valid
	def verifyBoard(board:List[List[Char]]) = {
		// * it should be a 3x3 board
		// * the value of each element must be any of 'X','O',' '
		board.length == 3 && board.forall(
			l => l.length == 3 && l.forall(e => List('X','O',' ').contains(e)))
	}

	def findWinnerOfBoard(board:List[List[Char]]) = {
		val possibleLines = List( 
			// search by row
			List( (0,0), (0,1), (0,2) ),
			List( (1,0), (1,1), (1,2) ),
			List( (2,0), (2,1), (2,2) ),
			// search by col
			List( (0,0), (1,0), (2,0) ),
			List( (0,1), (1,1), (2,1) ),
			List( (0,2), (1,2), (2,2) ),
			// search by diag
			List( (0,0), (1,1), (2,2) ),
			List( (0,2), (1,1), (2,0) ))

			
		def chooseWinner(curWinner:Char, curLine:List[(Int,Int)]) = {
			if (curWinner == 'X' || curWinner == 'O')
				curWinner
			else
			{
				val boardCells = curLine.map( xy => board(xy._1)(xy._2) ) 		
				if      ( boardCells.forall( e => e == 'X') )
					'X'
				else if ( boardCells.forall( e => e == 'O') )
					'O'
				else
					' '
			}

		}
		// try to fold and find a line
		val winner = possibleLines.foldLeft(' ')(chooseWinner)/* if we cannot find any winner for a while, leave it blank temporarily */
		println( "winner: " + winner )
	}


	def judgeBoard(board:List[List[Char]]) = {
		print("judge: ")

		// judge if it is a valid board
		if (!verifyBoard(board))
		{
			println( "invalid board" )
			false
		}
		findWinnerOfBoard(board)

	}
}


new TicTacToe().judgeBoard( List(
		List('X','O','X'),
		List('X',' ','O'),
		List('X','X',' ')))
