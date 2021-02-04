object ScrabbleScore {
    private val scoreMap: Map<Char, Int> = listOf(
        "AEIOULNRST" to 1,
        "DG" to 2,
        "BCMP" to 3,
        "FHVWY" to 4,
        "K" to 5,
        "JX" to 8,
        "QZ" to 10
    ).flatMap { (ks, score) ->
        ks.flatMap { k ->
            // cover both uppercase and lowercase.
            listOf(k to score, k.toLowerCase() to score)
        }
    }.toMap()

    private fun scoreLetter(c: Char): Int = scoreMap[c] ?: 0

    fun scoreWord(word: String): Int = word.sumBy(::scoreLetter)
}
