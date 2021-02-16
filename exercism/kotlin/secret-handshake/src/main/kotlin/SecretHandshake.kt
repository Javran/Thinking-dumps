typealias SignalOp = (List<Signal>) -> List<Signal>

object HandshakeCalculator {
    private val ops: List<Pair<Int, SignalOp>> = listOf(
        // Type inference seems to have trouble figuring out types for
        // lambdas with implicit argument `it`,
        // so I settled on using the explicit version.
        1 to { xs -> xs + Signal.WINK },
        0b10 to { xs -> xs + Signal.DOUBLE_BLINK },
        0b100 to { xs -> xs + Signal.CLOSE_YOUR_EYES },
        0b1000 to { xs -> xs + Signal.JUMP },
        0b10000 to { xs -> xs.asReversed() }
    )

    fun calculateHandshake(number: Int): List<Signal> =
        ops.fold(emptyList(), { acc, (mask, op) ->
            if (number and mask != 0) {
                op(acc)
            } else {
                acc
            }
        })
}