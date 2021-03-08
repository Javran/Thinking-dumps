class RotationalCipher(val offset: Int) {
    fun overInt(f: (Int) -> Int): (Char) -> Char = { ch ->
        when {
            ch.isLowerCase() -> ('a' + f(ch - 'a'))
            ch.isUpperCase() -> ('A' + f(ch - 'A'))
            else -> ch
        }
    }

    fun encode(text: String): String =
        text.map(overInt { (it + offset).rem(26) }).joinToString("")
}
