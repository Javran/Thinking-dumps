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

* `e1` runs first and then `e2`:
    * `x` is set to `100` after `e1` is done
    * `x` is set to `101` after `e2` is done
* `e1` runs its calculation part, then `e2` runs, `e1` changes `x` first
    * `x` is set to `100` after `e1` is done
    * `x` is set to `11` after `e2` is done
* `e1` runs its calculation part, then `e2` runs, `e2` changes `x` first
    * `x` is set to `11` after `e2` is done
    * `x` is set to `100` after `e1` is done
* `e2` runs first, then `e1`'s calculation part is blocked before `e2` is done.
    * `x` is set to `11` after `e2` is done
    * `x` is set to `121` after `e1` is done
