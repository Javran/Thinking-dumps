import java.lang.Exception

/*
  Extra notes regarding Forth as expected by testcases:

  - words are case-insensitive, this interpreter normalizes all symbols to uppercase.
  - static scoping: when a word is being defined, the enviroment is kept as it is (i.e. closure required).
  - the word itself is not considered in the definition of that word
    (i.e. a previous definition of the same word may be picked)
  - primitive words and operations are subject to shadowing.
  - unclear whether nested defnitions are supported, assuming no.
  - having exact checks on error message while having a statically typed implementing language
    with rich type of Exceptions is not just stupid, it is **f__king** stupid.

  Primitive spec:
  - arithmetic binary operator <op>: <stack>, a, b => <stack>, c, where <op> is +, -, *, / and c = <op> a b
  - DUP: <stack>, a => <stack>, a, a
  - DROP: <stack>, a => <stack>
  - SWAP: <stack>, a, b => <stack>, b, a
  - OVER: <stack>, a, b => <stack>, a, b, a

 */

// Runtime environment
typealias Env = Map<String, Forth.Action>
// Machine state
typealias Store = List<Int>
// Execution context
typealias Machine = Pair<Env, Store>

class Forth {

    // Parsed instructions.
    sealed class Instr {
        class Num(val v: Int) : Instr()
        class Op(val name: String) : Instr()
        class WordDef(val name: String, val body: List<Instr>) : Instr()
    }

    // Runtime actions bound to defined words.
    sealed class Action: (Machine) -> Machine {
        // The invoke() here is more powerful than necessary as it operations on Machine rather than just Store,
        // which opens the possiblity to support nested word definitions.

        class Prim(val exec: (Machine) -> Machine) : Action() {
            override operator fun invoke(m: Machine): Machine = exec(m)
        }

        class Closure(val env: Env, val body: List<Instr>) : Action() {
            override operator fun invoke(m: Machine): Machine = m.let { (menv, st) ->
                interpret(env to st, body).let { (_, st1) ->
                    // recover machine environment but keep global state after body is executed.
                    menv to st1
                }
            }
        }
    }

    // Parse and evaluate a single line, which is either a word definition or a sequence of instructions.
    private fun evaluate(m: Machine, line: String): Machine = interpret(m, parseLine(line))

    fun evaluate(vararg line: String): List<Int> = line.fold(initMachine, ::evaluate).second

    companion object {
        private val initEnv: Env = run {
            fun withStore(f: (Store) -> Store): (Machine) -> Machine =
                { (env, st) -> env to f(st) }

            fun pop(st: Store): Pair<Int, Store> {
                if (st.isEmpty()) {
                    throw Exception("empty stack")
                }
                return st.last() to st.dropLast(1)
            }

            fun defineUnary(op: (Store, Int) -> Store): Action =
                Action.Prim(withStore { st ->
                    run {
                        val (a, st0) = pop(st)
                        op(st0, a)
                    }
                })

            fun defineBinary(op: (Store, Int, Int) -> Store): Action =
                Action.Prim(withStore { st ->
                    run {
                        if (st.size == 1) {
                            throw Exception("only one value on the stack")
                        }
                        val (a, st0) = pop(st)
                        val (b, st1) = pop(st0)
                        op(st1, b, a)
                    }
                })

            mapOf(
                "+" to defineBinary { st, a, b -> st + (a + b) },
                "-" to defineBinary { st, a, b -> st + (a - b) },
                "*" to defineBinary { st, a, b -> st + (a * b) },
                "/" to defineBinary { st, a, b ->
                    if (b == 0) {
                        throw Exception("divide by zero")
                    } else {
                        st + (a / b)
                    }
                },
                "DUP" to defineUnary { st, a -> st + a + a },
                "DROP" to defineUnary { st, _ -> st },
                "SWAP" to defineBinary { st, a, b -> st + b + a },
                "OVER" to defineBinary { st, a, b -> st + a + b + a }
            )
        }

        val initMachine: Machine = initEnv to emptyList()

        // An atom is the minimum unit of Forth syntax excluding ':' and ';'.
        private fun parseAtom(atom: String): Instr = when (val v = atom.toIntOrNull(10)) {
            null -> Instr.Op(atom.toUpperCase())
            else -> Instr.Num(v)
        }

        fun parseLine(line: String): List<Instr> {
            // A line consists of either a definition or a sequence of instructions.
            val parts = line.split(" ")
            if (parts.isEmpty()) {
                return emptyList()
            }
            if (parts.first() == ":") {
                if (parts.last() != ";") {
                    throw Exception("Expected line to end with ';'")
                }
                if (parts.size <= 2) {
                    throw Exception("No word name given.")
                }
                val wordName = parts[1]
                if (wordName.all { it.isDigit() }) {
                    throw Exception("illegal operation")
                }
                return listOf(Instr.WordDef(wordName.toUpperCase(), parts.subList(2, parts.size - 1).map(::parseAtom)))
            }

            return parts.map(::parseAtom)
        }

        fun interpret(m: Machine, instrs: List<Instr>) = instrs.fold(m, ::interpret)

        fun interpret(m: Machine, instr: Instr): Machine = m.let { (env, st) ->
            when (instr) {
                is Instr.Num -> env to st + instr.v
                is Instr.Op -> when (val clo = env[instr.name]) {
                    null -> throw Exception("undefined operation")
                    else -> clo(m)
                }
                is Instr.WordDef -> run {
                    val newEnv = env + (instr.name to Action.Closure(env, instr.body))
                    newEnv to st
                }
            }
        }
    }
}
