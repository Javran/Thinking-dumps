import java.util.*

class Robot(private val id: Long) {

    constructor() : this(
        nextId()
    )

    val name: String
        get() = getRobotName(id)

    fun reset() = invalidateRobotName(id)

    companion object {
        private var rnd: Random = Random()
        // Since robot can be renamed, identity can only be establish by reference to object
        // or by maintaining an id pool.
        private var idPool: Long = 0
        // Keeps track of names of all existing robots.
        private var namePool: MutableSet<String> = mutableSetOf()
        // Current robot id to name mapping.
        private var names: MutableMap<Long, String> = mutableMapOf()
        // After a robot is renamed, its old names can no longer be used (for that robot).
        private var usedNames: MutableMap<Long, MutableSet<String>> = mutableMapOf()

        fun nextId(): Long {
            val id = idPool
            ++idPool
            return id
        }

        private fun createNewName(id: Long): String {
            val robotUsedNames: MutableSet<String> = when (val m = usedNames[id]) {
                null -> {
                    val n: MutableSet<String> = mutableSetOf()
                    usedNames[id] = n
                    n
                }
                else -> {
                    m
                }
            }
            while (true) {
                val l0: Char = 'A' + rnd.nextInt(26)
                val l1: Char = 'A' + rnd.nextInt(26)
                val num: Int = rnd.nextInt(1000)
                val newName = "$l0$l1${num.toString().padStart(3, '0')}"
                if (newName !in namePool && newName !in robotUsedNames) {
                    namePool.add(newName)
                    robotUsedNames.add(newName)
                    names[id] = newName
                    return newName
                }
            }
        }

        fun getRobotName(id: Long): String =
            when (val n = names[id]) {
                null -> createNewName(id)
                else -> n
            }

        fun invalidateRobotName(id: Long) = names[id]?.let { n ->
            namePool.remove(n)
            names.remove(id)
        }
    }
}
