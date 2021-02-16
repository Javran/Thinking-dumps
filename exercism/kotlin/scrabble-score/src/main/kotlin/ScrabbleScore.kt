object ScrabbleScore {
    private val scoreMap: Map<Char, Int> = run {
        val m: MutableMap<Char, Int> = mutableMapOf()
        for ((ks, score) in listOf(
            "AEIOULNRST" to 1,
            "DG" to 2,
            "BCMP" to 3,
            "FHVWY" to 4,
            "K" to 5,
            "JX" to 8,
            "QZ" to 10
        )) {
            for (k in ks) {
                // cover both uppercase and lowercase.
                m[k] = score
                m[k.toLowerCase()] = score
            }
        }
        m.toMap()
    }

    private fun scoreLetter(c: Char): Int = scoreMap[c] ?: 0

    fun scoreWord(word: String): Int = word.sumBy(::scoreLetter)
}
