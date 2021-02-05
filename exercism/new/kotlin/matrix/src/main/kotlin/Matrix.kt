class Matrix(private val matrixAsString: String) {

    // Parsed matrix in row-major order.
    private val mat: Array<IntArray> =
        matrixAsString
            .split("\n")
            .map { rawLine ->
                rawLine.trim().split(Regex("""\s+""")).map {
                    it.toInt(10)
                }.toIntArray()
            }.toTypedArray()

    fun column(colNr: Int): List<Int> = mat.map { it[colNr - 1] }

    fun row(rowNr: Int): List<Int> = mat[rowNr - 1].toList()
}
