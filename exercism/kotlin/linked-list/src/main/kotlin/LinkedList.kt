class Deque<T> {
    // The pivot node is the only node that does not carry any value
    // which also serves as both the node before first element and the node after last element.
    private val pivot: Node<T> = Node<T>(null)

    init {
        pivot.prev = pivot
        pivot.next = pivot
    }

    fun push(value: T) = insertAfter(pivot.prev, value)

    fun pop(): T? = remove(pivot.prev)

    fun unshift(value: T) = insertAfter(pivot, value)

    fun shift(): T? = remove(pivot.next)

    private fun remove(n: Node<T>): T? {
        if (n === pivot) {
            // this suggests we are operating on an empty collection,
            // in which case null is returned and there is no further action.
            return null
        }
        n.next.prev = n.prev
        n.prev.next = n.next
        return n.v!!
    }

    private fun insertAfter(x: Node<T>, v: T) {
        val n = Node(v)
        n.next = x.next
        n.prev = x
        x.next.prev = n
        x.next = n
    }

    private class Node<T>(val v: T?) {
        lateinit var prev: Node<T>
        lateinit var next: Node<T>
    }
}
