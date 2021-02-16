class IsbnVerifier {

    fun isValid(raw: String): Boolean =
        raw.filter { it != '-' }.let {
            it.length == 10 &&
                    it.toList().zip(10 downTo 1).flatMap { (ch, coeff) ->
                        when (ch) {
                            in '0'..'9' -> listOf((ch - '0') * coeff)
                            'x', 'X' -> {
                                if (coeff == 1) {
                                    listOf(10 * coeff)
                                } else {
                                    emptyList()
                                }
                            }
                            else -> emptyList()
                        }
                    }.let { converted ->
                        converted.size == 10 && converted.sum() % 11 == 0
                    }
        }

}
