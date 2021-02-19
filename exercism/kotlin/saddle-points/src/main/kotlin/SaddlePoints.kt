data class MatrixCoordinate(val row: Int, val col: Int)

class Matrix(mat: List<List<Int>>) {

    val saddlePoints: Set<MatrixCoordinate> = run {
        val rows = mat.size
        if (rows == 0) {
            return@run emptySet()
        }
        val cols = mat.first().size
        val rowMaxs: List<Int> = mat.map { it.max() ?: Int.MIN_VALUE }
        val colMins: List<Int> = List(cols) { c -> mat.map { it[c] }.min() ?: Int.MAX_VALUE }

        mat.mapIndexed { r, row ->
            row.mapIndexed { c, v ->
                if (v == rowMaxs[r] && v == colMins[c]) {
                    listOf(MatrixCoordinate(r + 1, c + 1))
                } else {
                    emptyList()
                }
            }.flatten()
        }.flatten().toSet()
    }
}