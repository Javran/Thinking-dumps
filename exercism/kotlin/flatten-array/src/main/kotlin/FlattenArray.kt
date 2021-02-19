object Flattener {
    fun flatten(source: Collection<Any?>): List<Any> =
        source.flatMap {
            when (it) {
                null -> emptyList()
                is Collection<Any?> -> flatten(it)
                else -> listOf(it)
            }
        }.toList()
}
