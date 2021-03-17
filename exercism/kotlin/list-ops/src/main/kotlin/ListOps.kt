// Just implementing everything with customFoldRight for fun :)
fun <T, U> List<T>.customFoldRight(initial: U, f: (T, U) -> U): U =
    if (this.isEmpty()) {
        initial
    } else {
        f(this.first(), this.drop(1).customFoldRight(initial, f))
    }

fun <T> List<T>.customAppend(list: List<T>): List<T> =
    this.customFoldRight(list, { hd, tl -> listOf(hd) + tl })

fun List<Any>.customConcat(): List<Any> = this.customFoldRight(emptyList(), { i, acc ->
    if (i is List<*>) {
        @Suppress("UNCHECKED_CAST")
        (i as List<Any>).customConcat()
    } else {
        listOf(i)
    } + acc
})

fun <T> List<T>.customFilter(predicate: (T) -> Boolean): List<T> =
    this.customFoldRight({ x: List<T> -> x }, { i, acc ->
        if (predicate(i)) {
            { listOf(i) + acc(it) }
        } else {
            acc
        }
    })(emptyList())

val List<Any>.customSize: Int get() = this.customFoldRight(0, { _, acc -> acc + 1 })

fun <T, U> List<T>.customMap(transform: (T) -> U): List<U> =
    this.customFoldRight(emptyList(), { i, acc -> listOf(transform(i)) + acc })

fun <T, U> List<T>.customFoldLeft(initial: U, f: (U, T) -> U): U =
    this.customFoldRight({ x: U -> x }, { i, acc -> { acc2 -> acc(f(acc2, i)) } })(initial)

fun <T> List<T>.customReverse(): List<T> =
    this.customFoldRight({ x: List<T> -> x }, { i, acc -> { acc(it) + i } })(emptyList())