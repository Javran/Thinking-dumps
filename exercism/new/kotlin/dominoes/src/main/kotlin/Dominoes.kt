class ChainNotFoundException(msg: String) : RuntimeException(msg)

data class Domino(val left: Int, val right: Int) {
    fun flip() = Domino(right, left)
}

object Dominoes {
    private fun search(current: Int, dominos: List<Domino>, path: List<Domino>): Sequence<List<Domino>> =
        sequence {
            if (dominos.isEmpty()) {
                // When all dominos are used, check whether a loop can be formed as last step.
                if (path.first().left == path.last().right) {
                    yield(path)
                }
            } else {
                val catDominos: List<Pair<Boolean, Domino>> = dominos.map { d ->
                    when {
                        d.left == current -> Pair(true, d)
                        d.right == current -> Pair(true, d.flip())
                        else -> Pair(false, d)
                    }
                }

                val dPair = catDominos.partition { it.first }
                val nextAltDominos = dPair.first.map { it.second }
                val nextUnusedDominos = dPair.second.map { it.second }
                for (x in nextAltDominos.withIndex()) {
                    val newDominos = nextAltDominos - x.value + nextUnusedDominos
                    yieldAll(search(x.value.right, newDominos, path + x.value))
                }
            }
        }

    fun formChain(inputDominoes: Iterable<Domino>): List<Domino> {
        val dominos = inputDominoes.toList()
        if (dominos.isEmpty()) {
            return emptyList()
        }
        return search(dominos.first().left, dominos, emptyList()).take(1).toList().let { result ->
            if (result.isEmpty()) {
                throw ChainNotFoundException("Solution not found.")
            }
            result.first()
        }
    }

    fun formChain(vararg inputDominoes: Domino): List<Domino> =
        formChain(inputDominoes.asIterable())
}
