// This introduces a notion of "time" - an event is a triggering update to a cell,
// if this system has feedback lopps, we'd better make sure a cell doesn't update itself indefinitely.
typealias EventId = Int

class Reactor<T>() {
    private var _maxId: Int = 0

    @Synchronized
    private fun newEventId(): Int = ++_maxId

    // Your compute cell's addCallback method must return an object
    // that implements the Subscription interface.
    interface Subscription {
        fun notify(t : EventId)
        fun cancel()
    }

    inner class BaseCell<T> {
        protected var _lastWrite: EventId = newEventId()

        protected val subscribers = mutableSetOf<Subscription>()

        var lastWrite: Int
            get() = _lastWrite
            private set(v) {
                _lastWrite = v
            }

    }

    inner class InputCell<T>(private var i: T) {
        private var _lastWrite: EventId = newEventId()

        private val subscribers = mutableSetOf<Subscription>()

        var lastWrite: Int
            get() = _lastWrite
            private set(v) {
                _lastWrite = v
            }

        var value: T
            get() {
                return i
            }
            set(v) {
                if (i == v) {
                    return
                }
                lastWrite = newEventId()
                subscribers.forEach { it.notify(lastWrite) }
            }

        fun addCallback(cb : (T) -> Unit) : Subscription = object : Subscription {
            var lastWrite = 0

            init {
                subscribers.add(this)
            }

            override fun notify(t: EventId) {
                if (t > lastWrite) {
                    cb(value)
                    lastWrite = t
                }
            }

            override fun cancel() {
                subscribers.remove(this)
            }

        }
    }



}
