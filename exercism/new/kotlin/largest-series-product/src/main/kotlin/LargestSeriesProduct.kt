import java.lang.IllegalArgumentException

class Series(private val input: String) {

    init {
        if (input.any { it !in '0'..'9' }) {
            throw IllegalArgumentException("Input string contains non-digit.")
        }
    }

    fun getLargestProduct(span: Int): Long =
        when {
            span < 0 -> throw IllegalArgumentException("Expect non-negative span.")
            span > input.length -> throw IllegalArgumentException("Span longer than input string.")
            span == 0 -> 1
            else ->
                input
                    .windowed(span)
                    .flatMap {
                        if ('0' !in it) {
                            listOf(Companion.seriesProductOf(it))
                        } else {
                            emptyList()
                        }
                    }.max() ?: 0
        }

    companion object {
        fun seriesProductOf(s: String): Long =
            s.map { it - '0' }.fold(1, Long::times)
    }
}
