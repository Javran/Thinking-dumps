object BeerSong {
    private fun verse(n: Int, notLast: Boolean): Sequence<String> = sequence {
        yield(
            when (n) {
                0 -> "No more bottles of beer on the wall, no more bottles of beer."
                1 -> "1 bottle of beer on the wall, 1 bottle of beer."
                else -> "$n bottles of beer on the wall, $n bottles of beer."
            }
        )
        yield(
            when (val r = n - 1) {
                -1 -> "Go to the store and buy some more, 99 bottles of beer on the wall."
                0 -> "Take it down and pass it around, no more bottles of beer on the wall."
                1 -> "Take one down and pass it around, 1 bottle of beer on the wall."
                else -> "Take one down and pass it around, $r bottles of beer on the wall."
            }
        )
        if (notLast) {
            yield("")
        }
    }

    fun verses(startBottles: Int, takeDown: Int): String =
        (startBottles downTo takeDown)
            .flatMap { i -> verse(i, i != takeDown).toList() }
            .joinToString("\n", postfix = "\n")
}
