object WordCount {

    // Tries to normalize current word by:
    // - drop it if it's an empty string
    // - removing surrounding quotations (if any)
    // - turning it to lowercase
    fun normalizeFlat(word: String): String? =
        when {
            word.isEmpty() -> null
            word.length >= 2 && word.first() == '\'' && word.last() == '\'' ->
                word.substring(1, word.lastIndex)
            else -> word
        }?.toLowerCase()

    fun phrase(phrase: String): Map<String, Int> =
        phrase.split("""[^\da-z\']+""".toRegex(RegexOption.IGNORE_CASE))
            .mapNotNull { normalizeFlat(it) }
            .groupBy { it }
            .mapValues { it.value.size }
}
