// Note that the problem template uses <out T> but I changed it to <T>
// because my solution does require consumption of the type for comparing and arithmetic operations.
class Triangle<T>(private val a0: T, private val b0: T, private val c0: T)
        where T : Comparable<T>,
              T : Number {
    // INVARIANT: a <= b <= c, where a, b, c are the same as a0, b0, c0 up to permutation.
    private val a: T
    private val b: T
    private val c: T

    val isEquilateral: Boolean
    val isIsosceles: Boolean
    val isScalene: Boolean

    init {
        fun sort(x: T, y: T) =
            if (x <= y) {
                x to y
            } else {
                y to x
            }

        /*
          Sorting network:
          a0 ----> a1 ----= a1 ----> a2
              x                 x
          b0 ----> b1 ----> b2 ----> b3
                       x
          c0 ----= c0 ----> c1 ----= c1

          "x" marks where a sorter is applied
         */
        val (a1, b1) = sort(a0, b0)
        val (b2, c1) = sort(b1, c0)
        val (a2, b3) = sort(a1, b2)

        a = a2
        b = b3
        c = c1

        if (c == 0) {
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
