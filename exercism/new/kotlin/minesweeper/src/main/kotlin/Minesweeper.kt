import java.lang.StringBuilder

/*
  To represent a minesweeper board, two obvious design choices are:
  (1) store the board in some consecutive manner (Array / List)
  (2) store coordinates of mines
  Use case is insufficient to tell which one is better, going for (2).
 */
data class MinesweeperBoard(val rawBoard: Collection<String>) {

    data class Coord(val row: Int, val col: Int) {
        // Returns coodinates covering 3x3 area of this Coord, including itself.
        fun coverings(): List<Coord> =
            (row - 1..row + 1).flatMap { r ->
                (col - 1..col + 1).map { c -> Coord(r, c) }
            }
    }

    private val rows: Int = rawBoard.size
    private val cols: Int
    private val mines: Set<Coord>

    /*
        mineCounts records number of mines in a 3x3 area centered at key Coord.

        Note that:
        - Out-of-bound Coords may be included as key
        - mines and mineCounts keys are not disjoint, i.e. forall c in mines, mineCounts[c] == 1
     */
    private val mineCounts: Map<Coord, Int>

    init {
        if (rows > 0) {
            cols = rawBoard.first().length
            if (rawBoard.any { it.length != cols }) {
                throw Exception("Column length differs.")
            }
            mines = rawBoard.mapIndexed { row, rawLine ->
                rawLine.mapIndexed { col, ch ->
                    if (ch == '*') {
                        listOf(Coord(row, col))
                    } else {
                        emptyList()
                    }
                }.flatten()
            }.flatten().toSet()
            // each mine contributes to the mine count of its covering (3x3) area.
            mineCounts = mines
                .flatMap { it.coverings() }
                .map { it to Unit }
                .groupBy { it.first }
                .mapValues { it.value.size }
        } else {
            cols = 0
            mines = emptySet()
            mineCounts = emptyMap()
        }
    }

    private fun getSquare(c: Coord): String =
        if (c in mines) {
            "*"
        } else {
            mineCounts[c]?.toString() ?: " "
        }

    fun withNumbers(): List<String> =
        (0 until rows).map { r ->
            (0 until cols).map { c ->
                getSquare(Coord(r, c))
            }.fold(StringBuilder(), StringBuilder::append).toString()
        }
}
