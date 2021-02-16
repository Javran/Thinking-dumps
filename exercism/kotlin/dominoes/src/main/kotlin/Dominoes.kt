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
                val (nextAltDominos, nextUnusedDominos) = run {
                    // split and transform current list of dominos into two parts:
                    // - the "left" part are all dominos usable as next domino
                    //   (might be flipped so that it.left == current)
                    // - the "right" part are all dominos unusable as next domino.
                    val catDominos: List<Pair<Boolean, Domino>> = dominos.map { d ->
                        when {
                            d.left == current -> true to d
                            d.right == current -> true to d.flip()
                            else -> false to d
                        }
                    }
                    val (left, right) = catDominos.partition { it.first }
                    left.map { it.second } to right.map { it.second }
                }

                for (d in nextAltDominos) {
                    val newDominos = nextAltDominos - d + nextUnusedDominos
                    yieldAll(search(d.right, newDominos, path + d))
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
