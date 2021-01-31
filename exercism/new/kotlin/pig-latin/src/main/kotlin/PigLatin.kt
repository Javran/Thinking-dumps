object PigLatin {

    val vowels: List<String> = listOf(
        "xr", "yt",
        "a", "e", "i", "o", "u"
    )

    val consonantClusters: List<String> = listOf(
        "sch", "thr",
        "ch", "qu", "th", "rh",
        "y"
    )

    fun splitFirstCluster(word: String): Triple<String, Boolean, String> {
        for (prev in vowels) {
            if (word.startsWith(prev)) {
                return Triple(prev, true, word.substring(prev.length))
            }
        }
        for (prev in consonantClusters) {
            if (word.startsWith(prev)) {
                return Triple(prev, false, word.substring(prev.length))
            }
        }
        return Triple(word.substring(0,1), false, word.substring(1))
    }

    fun translateWord(word: String): String {
        val (head, headIsVowel, tail) = splitFirstCluster(word)
        when {
            headIsVowel ->
                // Rule 1
                return word + "ay"
            tail.startsWith("y") ->
                // Rule 4
                return tail + head + "ay"
            tail.startsWith("qu") ->
                // Rule 3
                return tail.substring(2) + head + "quay"
            else ->
                // Rule 2
                return tail + head + "ay"
        }
    }

    fun translate(phrase: String): String =
        phrase.split(" ").map(::translateWord).joinToString(" ")
}
