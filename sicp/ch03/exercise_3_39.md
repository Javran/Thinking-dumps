Exercise 3.39: Which of the five possibilities in the parallel execution
shown above remain if we instead serialize execution as follows:

    (define x 10)
    (define s (make-serializer))
    (parallel-execute
      (lambda ()
        (set! x
          ((s (lambda () (* x x))))))
      (s (lambda () (set! x (+ x 1)))))

Answer:

The code above enforces that two events can not
happen at the same time:

* calculating `(* x x)`
* calculating `(+ x 1)` and setting `x`

There are 2 executions, let me name them `e1` and `e2` respectively.

We also name all the operations as following:

* `e1` =  `(lambda () (* x x))` + `(set! x <result>)` = `a` + `b`
* `e2` =  `(lambda () (+ x 1))` + `(set! x <result>)` = `c` + `d`

And the constraint can be described as:

* `a` cannot interrupt between `c` and `d`.
* `a` -> `b` is enforced by the order of function application 
* `c` -> `b` is enforced by the order of function application 

We begin with ignoring the first constraint and then test
whether the first constriant is met or not.

All possibilities:

* Case #1: `a` -> `b` -> `c` -> `d`
    * `x=10` for a
    * `x` is changed to `100` in `b`
    * `x=100` for `c`
    * `x` is changed to `101` in `d`
    * finally `x=101`
* Case #2: `a` -> `c` -> `b` -> `d`
    * `x=10` for `a`
    * `x=10` for `c`
    * `x` is changed to `100` in `b`
    * `x` is changed to `11` in `c`
    * finally `x=11`
* Case #3: `a` -> `c` -> `d` -> `b`
    * `x=10` for `a`
    * `x=10` for `c`
    * `x` is changed to `11` in `c`
    * `x` is changed to `100` in `b`
    * finally `x=100`
* Case #4: `c` -> `a` -> `b` -> `d`
    * when `c` is getting executed, `a` has to wait until `d` is done
    * so this case is not allowed
    * or we say that this case is enforced to be the same as `c` -> `d` -> `a` -> `b`
* Case #5: `c` -> `a` -> `d` -> `b`
    * when `c` is getting executed, `a` has to wait until `d` is done
    * so this case is not allowed
    * or we say that this case is enforced to be the same as `c` -> `d` -> `a` -> `b`
* Case #6: `c` -> `d` -> `a` -> `b`
    * `x=10` for `c`
    * `x` is changed to `11` in `d`
    * `x=11` for `a`
    * `x` is changed to `121` in `b`
    * finally `x=121`

As the question say that there are 5 possibilities, we can merge Case #4 and Case #5 together.

And now we see that all possible values after a full execution are `101,11,100,121`.
