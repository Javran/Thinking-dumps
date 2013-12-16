Suppose we have a scenario that:

* Paul and Peter share a bank account `acc`
* Peter wants to deposit $100 to the bank account
* Paul wants to withdraw $50 from the bank account
* Bank account has initialized with $200
* The correct answer will be: `200+100-50 = 200-50+100 = 250`

The not serialized version of implementation might be:

    ...
    (define (deposit amount)
      (set! balance
            (+ balance amount)))
    (deposit (withdraw amount)
      (set! balance
            (- balance amount)))
    ...

And then serialized version adds `(<mutex> 'acquire)` before
any operation is done and `(<mutex> 'release)` after
all operations are done.

Let's make a list for the possible instructions need by Peter:

1. `acquire`
2. `test-and-set!` *(not atomic)*
3. `(car cell)`, `goto 5` on true
4. `set-car!`
5. `goto 1` if `test-and-set!` returns true
6. `(+ balance amount)`, with amount bound to 100
7. `set!`
8. `release`

And instructions for Paul:

1. `acquire`
2. `test-and-set!` *(not atomic)*
3. `(car cell)`, `goto 5` on true
4. `set-car!`
5. `goto 1` if `test-and-set!` returns true
6. `(- balance amount)`, with amount bound to 50
7. `set!`
8. `release`

Let `A:` be Peter and `B:` be Paul,
consider the execution of the following sequence of instructions:

* A: `acquire`
* B: `acquire`
* A: `test-and-set!` *(not atomic)*
* B: `test-and-set!` *(not atomic)*
* A: `(car cell)`, will return false
* B: `(car cell)`, will return false
* A: `(begin (set-car! cell true) false)` 
* B: `(begin (set-car! cell true) false)`
* A: `(+ balance amount)`, result = 300
* B: `(- balance amount)`, result = 150
* A: `(set! balance 300)`
* B: `(set! balance 150)`
* A: `release`
* B: `release`

Finally `balance = 150`, incorrect.

However, if `test-and-set!` is atomic:

* A: `acquire`
* B: `acquire`
* A: `test-and-set!`
* B: `test-and-set!` (not allowed, has to wait)
* A: `(car cell)`, will return false
* A: `(begin (set-car! cell true) false)` 
* B: `acquire`
* B: `test-and-set!` (returned true, retrying)
* A: `(+ balance amount)`, result = 300
* A: `(set! balance 300)`
* B: `test-and-set!` (returned true, retrying)
* A: `release`
* B: `test-and-set!`
* B: `(car cell)`, will return false
* B: `(begin (set-car! cell true) false)`
* B: `(- balance amount)`, result = 250
* B: `(set! balance 250)`
* B: `release`

The result should be correct now.
