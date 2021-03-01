object MatchingBrackets {
    fun isValid(input: String): Boolean {
        // expectedRights maintains a stack of right parentheses that are expected to be the next.
        fun verify(i: Int, expectedRights: List<Char>): Boolean = when (val ch = input.getOrNull(i)) {
            null -> expectedRights.isEmpty()
            '(' -> verify(i + 1, expectedRights + ')')
            '[' -> verify(i + 1, expectedRights + ']')
            '{' -> verify(i + 1, expectedRights + '}')
            ')', ']', '}' -> expectedRights.lastOrNull() == ch && verify(i + 1, expectedRights.dropLast(1))
            else -> verify(i + 1, expectedRights)
        }
        return verify(0, emptyList())
    }
}
