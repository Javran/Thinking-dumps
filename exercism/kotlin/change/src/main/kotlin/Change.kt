class ChangeCalculator(val coins: List<Int>) {
    fun computeMostEfficientChange(grandTotal: Int): List<Int> {
        if (grandTotal < 0) {
            throw IllegalArgumentException("Negative totals are not allowed.")
        }
        // The presence of a key means that total has been search before and memoized.
        val memo: MutableMap<Int, List<Int>?> = mutableMapOf()
        memo[0] = emptyList()
        fun findChange(total: Int): List<Int>? {
            if (total < 0) {
                return null
            }
            // I would love to use when-expression.
            // However here null value could mean "key not found" or "key is found and the value is null",
            // and I don't know whether it's possible to disinguish them.
            if (memo.containsKey(total)) {
                return memo[total]
            }
            val result: List<Int>? =
                coins.mapNotNull { coin -> findChange(total - coin)?.let { xs -> xs + coin } }.minBy { it.size }
            memo[total] = result
            return result
        }
        return findChange(grandTotal)
            ?: throw IllegalArgumentException("The total $grandTotal cannot be represented in the given currency.")
    }
}

