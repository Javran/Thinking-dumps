#!/usr/bin/env scala
!#

// Task #1: Tic-Tac-Toe Game
object TicTacToe {

	// judge whether a board is valid
	def verifyBoard(board:List[List[Char]]) = {
		// * it should be a 3x3 board
		// * the value of each element must be any of 'X','O',' '
		if (board.length == 3 && 
			board.forall(l => l.length == 3 && l.forall(e => List('X','O',' ').contains(e)))) {
			val flattenBoard = board.flatten
			val countX = flattenBoard.filter( e => e == 'X' ).length
			val countO = flattenBoard.filter( e => e == 'O' ).length
			countX == countO || (countX - 1) == countO
		}
		else
			false
	}

	/*
		returns:
			' ' 		if I cannot judge who is the winner
			'X'/'O' 	if I can find the winner
			'*' 		elsewise(both players own a line...)
	*/
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

		// try to fold and find a line
		possibleLines
			/* if we cannot find any winner for a while, leave it blank temporarily */
			.foldLeft(' ') (
				(curWinner:Char, curLine:List[(Int,Int)]) => {
					if (curWinner == '*')
						curWinner
					else
					{
						val boardCells = curLine.map( xy => board(xy._1)(xy._2) ) 		
						if      ( boardCells.forall( e => e == 'X') )
						{
							if (curWinner == 'O')
								'*'
							else
								'X'
						}
						else if ( boardCells.forall( e => e == 'O') )
						{
							if (curWinner == 'X')
								'*'
							else
								'O'
						}
						else
							curWinner
					}

				}

			
			)
	}

	def isFullBoard(board:List[List[Char]]) = {
		board.forall( row => row.forall( e => e != ' ') )	
	}

	/*
		summary:
			return judge result
		returns:
			'invalid' 	if the board is invalid
			'X' or 'O' 	if I can figure out the winner
			'tie' 		if the board is full but no one wins
			'both' 		if both player owns a line...
			'incomplete' 	elsewise	
	*/
	def judgeBoard(board:List[List[Char]]) = {
		// judge if it is a valid board
		if (!verifyBoard(board))
			"invalid"
		else
		{
			val winner = findWinnerOfBoard(board)
			if ( winner != ' ' )
			{
				if (winner == '*')
					"both"
				else
					/*
						note here is a Char, but we need a string
					*/
					winner.toString()
			}
			else
			{
				if (isFullBoard(board))
					"tie"
				else
					"incomplete"
			}
		}

	}

	/*
		summary:
			parse raw data like:
				List( "XOX", "OXO", "XOX")
			into a valid board
		returns:
			return Nil when parsing failed or the board is invalid
	*/
	def parseBoard(raw:List[String]) = {
		try {
			val board = raw.map( row => List(0,1,2).map( ind => row(ind) ) )
			if (!verifyBoard(board))
				Nil
			else
				board
		} catch {
			case ex: Exception => {
				Nil
			}
		}
	}
}

def testBoard(raw: List[String], caseStr: String, expectedStr: String) = {
	println( ">>> test case: " + caseStr )
	println( "Board tester: raw data is: " + raw ) 
	val board = TicTacToe.parseBoard( raw )
	if (board == Nil)
	{
		println( "=== Board tester: parsing failed" )
	}
	else
	{
		val status = TicTacToe.judgeBoard( board )
		println( "Board tester: judger  returns: [" + status + "]" )
		println( "Board tester: expected return: [" + expectedStr + "]" )
		println( "=== expected result? " + (status == expectedStr) )
	}
	println( "<<< test case done" )
}

testBoard( List(""), 
	"invalid", "invalid")

testBoard( List(),
	"invalid", "invalid")

testBoard( List(
	"   ",
	"   ",
	"   "), "empty board", "incomplete")

testBoard( List(
	"X O",
	"   ",
	"O X"), "incomplete board", "incomplete")

testBoard( List(
	"X O",
	" X ",
	"O X"), "winner X", "X")

testBoard( List(
	"OXO",
	"XXO",
	"XOX"), "tie situation", "tie")

testBoard( List(
	"X X",
	"OOO",
	"XXO"), "winner O", "O")

testBoard( List(
	"XXX",
	"OOO",
	"XXO"), "both win(impossible)", "both")

testBoard( List(
	"X X",
	"OOO",
	"X O"), "Count X < Count O", "invalid")
