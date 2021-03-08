import java.lang.IllegalArgumentException

object Series {
    fun slices(n: Int, s: String): List<List<Int>> = if (s.isEmpty() || n > s.length) {
        throw IllegalArgumentException()
    } else {
         s.windowed(n).map { it.map { ch -> ch - '0' } }
    }
}
