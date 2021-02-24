object WordCount {

    fun phrase(phrase: String): Map<String, Int> =
        phrase.split("""[^\da-z\']+""".toRegex(RegexOption.IGNORE_CASE))
            .mapNotNull {
                if (it.isBlank()) {
                    null
                } else {
                    it.removeSurrounding("'").toLowerCase()
                }
            }
            .groupBy { it }
            .mapValues { it.value.size }
}
