class Matrix(private val matrixAsString: String) {

    // Parsed matrix in row-major order.
    private val mat: List<List<Int>> =
        matrixAsString
            .lines()
            .map { rawLine ->
                Regex("""(\d+)""")
                    .findAll(rawLine)
                    .map { it.groupValues[1].toInt(10) }
                    .toList()
            }

    fun column(colNr: Int): List<Int> = mat.map { it[colNr - 1] }

    fun row(rowNr: Int): List<Int> = mat[rowNr - 1]
}
