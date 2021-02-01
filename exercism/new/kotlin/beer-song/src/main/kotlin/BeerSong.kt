object BeerSong {
    fun verses(startBottles: Int, takeDown: Int): String {
        val xs: MutableList<String> = mutableListOf()
        for (i in startBottles downTo takeDown) {
            xs.add(
                when (i) {
                    0 -> "No more bottles of beer on the wall, no more bottles of beer."
                    1 -> "1 bottle of beer on the wall, 1 bottle of beer."
                    else -> "$i bottles of beer on the wall, $i bottles of beer."
                }
            )
            xs.add(
                when (val j = i - 1) {
                    -1 -> "Go to the store and buy some more, 99 bottles of beer on the wall."
                    0 -> "Take it down and pass it around, no more bottles of beer on the wall."
                    1 -> "Take one down and pass it around, 1 bottle of beer on the wall."
                    else -> "Take one down and pass it around, $j bottles of beer on the wall."
                }
            )
            if (i != takeDown) {
                xs.add("")
            }
        }
        return xs.joinToString("\n", postfix = "\n")
    }
}
