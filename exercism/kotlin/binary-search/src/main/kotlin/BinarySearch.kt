import kotlin.math.sign

object BinarySearch {
    fun search(list: List<Int>, item: Int): Int {
        fun searchInRange(startInd: Int, endIndExcl: Int): Int {
            if (endIndExcl <= startInd) {
                throw NoSuchElementException()
            }
            val mid = (startInd + endIndExcl) / 2
            return when (list[mid].compareTo(item).sign) {
                0 -> mid
                -1 -> searchInRange(mid + 1, endIndExcl)
                1 -> searchInRange(startInd, mid)
                else -> error("Unreachable")
            }
        }
        return searchInRange(0, list.size)
    }
}
