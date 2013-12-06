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

all possible combinations are:

* `a -> b -> c -> d -> e -> f -> g`
* `a -> b -> d -> c -> e -> f -> g`
* `a -> b -> d -> e -> c -> f -> g`
* `a -> b -> d -> e -> f -> c -> g`
* `a -> b -> d -> e -> f -> g -> c`
* `a -> d -> b -> c -> e -> f -> g`
* `a -> d -> b -> e -> c -> f -> g`
* `a -> d -> b -> e -> f -> c -> g`
* `a -> d -> b -> e -> f -> g -> c`
* `a -> d -> e -> b -> c -> f -> g`
* `a -> d -> e -> b -> f -> c -> g`
* `a -> d -> e -> b -> f -> g -> c`
* `a -> d -> e -> f -> b -> c -> g`
* `a -> d -> e -> f -> b -> g -> c`
* `a -> d -> e -> f -> g -> b -> c`
* `d -> a -> b -> c -> e -> f -> g`
* `d -> a -> b -> e -> c -> f -> g`
* `d -> a -> b -> e -> f -> c -> g`
* `d -> a -> b -> e -> f -> g -> c`
* `d -> a -> e -> b -> c -> f -> g`
* `d -> a -> e -> b -> f -> c -> g`
* `d -> a -> e -> b -> f -> g -> c`
* `d -> a -> e -> f -> b -> c -> g`
* `d -> a -> e -> f -> b -> g -> c`
* `d -> a -> e -> f -> g -> b -> c`
* `d -> e -> a -> b -> c -> f -> g`
* `d -> e -> a -> b -> f -> c -> g`
* `d -> e -> a -> b -> f -> g -> c`
* `d -> e -> a -> f -> b -> c -> g`
* `d -> e -> a -> f -> b -> g -> c`
* `d -> e -> a -> f -> g -> b -> c`
* `d -> e -> f -> a -> b -> c -> g`
* `d -> e -> f -> a -> b -> g -> c`
* `d -> e -> f -> a -> g -> b -> c`
* `d -> e -> f -> g -> a -> b -> c`
