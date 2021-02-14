// Note that the problem template uses <out T> but I changed it to <T>
// because my solution does require consumption of the type for comparing and arithmetic operations.
class Triangle<T>(private val t: Triple<T, T, T>)
        where T : Comparable<T>,
              T : Number {
    // INVARIANT: t.a <= t.b <= t.c, where a, b, c are the same as a0, b0, c0 up to permutation.
    constructor(a0: T, b0: T, c0: T) : this(
        listOf(a0, b0, c0)
            .sorted().let { (a, b, c) -> Triple(a, b, c) }
    )

    val isEquilateral: Boolean
    val isIsosceles: Boolean
    val isScalene: Boolean

    init {
        val (a, b, c) = t
        if (a == 0) {
            throw IllegalArgumentException("All sides are non-positive.")
        }
        // No good solution here to invoke type-specific arithmetic methods:
        // - Kotlin reflection would require extra dependency
        // - Something like https://discuss.kotlinlang.org/t/how-to-write-generic-functions-for-all-numeric-types/7367
        //   would work, but it's repetitive.
        // - Settle on toDouble(), which is compatible with most of other types
        //   Long.toDouble() might throw out some least significant bits for values taking more than 52bits
        //   therefore needs special handling.
        val isTriangle = when (a) {
            is Long ->
                // a + b > c but rearranged to avoid overflow.
                a.toLong() > c.toLong() - b.toLong()
            else -> a.toDouble() + b.toDouble() > c.toDouble()
        }
        if (!isTriangle) {
            throw IllegalArgumentException("Triangle inequality violated")
        }
        isEquilateral = a == c
        isIsosceles = a == b || b == c
        isScalene = !isIsosceles

    }
}
