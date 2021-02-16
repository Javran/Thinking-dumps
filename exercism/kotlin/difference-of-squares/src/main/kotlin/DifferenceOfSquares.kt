class Squares(private val n: Int) {
    // computes 1^2 + 2^2 + ... + n^2 given n.
    fun sumOfSquares(): Int =
        // http://oeis.org/A000330
        n * (n + 1) * (n + n + 1) / 6

    // computes (1 + 2 + ... + n)^2 given n.
    fun squareOfSum(): Int =
        (n * (n + 1) / 2).let { it * it }

    // computes squareOfSum() - sumOfSquares()
    fun difference(): Int =
        // http://oeis.org/A052149
        n * (n - 1) * (n + 1) * (3 * n + 2) / 12
}
