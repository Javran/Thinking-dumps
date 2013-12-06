Give all possible values of x that can result
from executing:

    (define x 10)
    (parallel-execute
      (lambda ()
        (set! x (* x x)))
      (lambda ()
        (set! x (* x x x))))

This might behave in various ways,
even how each term is evaluated matters.

* Case #1: A term is evaluted only once for each s-expr
  (i.e. for `(* x x)`, `x` is only evaluated once
  and the return value is cached.
   
* Case #2: A term might be evaluated multiple times for
  one single s-expr

For *Case #1*:

Let's break two lambdas into:

    lambda #1 = calc -> set! = a -> b
    lambda #2 = calc -> set! = c -> d

And all possible combinations are:

* `a -> b -> c -> d`
    * `x=100` after `b` is done
    * `x=1000000` after `d` is done
    * finally `x=1000000`
* `a -> c -> b -> d`
    * `x=100` after `b` is done
    * `x=1000` after `d` is done
    * finally `x=1000`
* `a -> c -> d -> b`
    * `x=1000` after `d` is done
    * `x=100` after `b` is done
    * finally `x=100`
* `c -> a -> b -> d`
    * `x=100` after `b` is done
    * `x=1000` after `d` is done
    * finally `x=1000`
* `c -> a -> d -> b`
    * `x=1000` after `d` is done
    * `x=100` after `b` is done
    * finally `x=100`
* `c -> d -> a -> b`
    * `x=1000` after `d` is done
    * `x=1000000` after `b` is done
    
So all possible values are:
    [1000000, 1000, 100]

For *Case #2*:

Let's first do some currying and make every operation "atmoic":

    (define x 10)
    (parallel-execute
      (lambda ()
        (set! x
          (((lambda (x1)
            (lambda (x2)
              (* x1 x2)))
            x ; first arg
            )
            x ; second arg
            )))
      (lambda ()
        (set! x
          ((((lambda (x1)
             (lambda (x2)
             (lambda (x3)
               (* x1 x2 x3))))
             x ; first arg
             )
             x : second arg
             )
             x ; third arg
             ))))

Now break two lambdas into:

    lambda #1 = fetch x1 -> fetch x2 & calc -> set! = a -> b -> c
    lambda #2 = fetch x1 -> fetch x2 -> fetch x3 & calc -> set! = d -> e -> f -> g

