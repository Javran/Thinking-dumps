object PigLatin {
    // List of char sequence that should be considered a single cluster of vowel / consonant.
    // Longer elements must appear first.
    private val clusters: List<Pair<String, Boolean>> = listOf(
        "sch" to false, "thr" to false,
        "xr" to true, "yt" to true,
        "ch" to false, "qu" to false, "th" to false, "rh" to false,
        "a" to true, "e" to true, "i" to true, "o" to true, "u" to true,
        "y" to false
    )

    private data class SplitResult(
        val head: String,
        val headIsVowel: Boolean,
        val tail: String
    )

    // Splits a non-empty word by first vowel / consonant cluster.
    // INVARIANT: head + tail == <input word>, where head and tail are from SplitResult.
    private fun splitFirstCluster(word: String): SplitResult {
        for ((prefix, prefixIsVowel) in clusters) {
            if (word.startsWith(prefix)) {
                return SplitResult(prefix, prefixIsVowel, word.substring(prefix.length))
            }
        }
        return SplitResult(word.substring(0, 1), false, word.substring(1))
    }

    // Translates a single word into Pig Latin. It is assumed that the word is not empty.
    private fun translateWord(word: String): String {
        val (head, headIsVowel, tail) = splitFirstCluster(word)
        return when {
            headIsVowel ->
                // Rule 1
                word + "ay"
            tail.startsWith("y") ->
                // Rule 4
                tail + head + "ay"
            tail.startsWith("qu") ->
                // Rule 3
                tail.substring(2) + head + "quay"
            else ->
                // Rule 2
                tail + head + "ay"
        }
    }

    fun translate(phrase: String): String =
        phrase.split(" ").map(::translateWord).joinToString(" ")
}
