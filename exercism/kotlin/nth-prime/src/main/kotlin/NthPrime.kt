import java.lang.IllegalArgumentException

object Prime {
    val primes: Sequence<Int> by lazy {
        sequence {
            yield(2)
            yield(3)
            yieldAll(generateSequence(5) { it + 2 }
                .filter {
                    primes.takeWhile { x -> x * x <= it }.all { x -> it % x != 0 }
                })
        }
    }

    fun nth(n: Int): Int {
        if (n <= 0) throw IllegalArgumentException("There is no zeroth prime.")
        return primes.elementAt(n - 1)
    }
}
