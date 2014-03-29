*Give all possible values of x that can result
from executing*:

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

Here what is interesting is when the mutation (i.e. `c` and `g`) happens.
So the following situations should result in the same value.

* `a -> b -> d -> e -> f -> c -> g` 
* `a -> d -> b -> e -> f -> c -> g` 
* `a -> d -> e -> b -> f -> c -> g` 

You might argue that the result of following might not be equal:

* `a -> b -> d -> c -> e -> f -> g`
* `a -> d -> b -> c -> e -> f -> g`
* `d -> a -> b -> c -> e -> f -> g`

But let's think `c` and `g` as seperators. 
As long as the order of `{valid perm of a, b, d} c {valid perm of e,f} g` holds, everything will be fine.

Using this idea, here are all the situations we want to consider:

* `c` happens before `g`
    * `a -> b -> c -> d -> e -> f -> g`
    * `a -> b -> d -> c -> e -> f -> g`
    * `a -> b -> d -> e -> c -> f -> g`
    * `a -> b -> d -> e -> f -> c -> g`

* `g` happens before `c`
    * `d -> e -> f -> g -> a -> b -> c`
    * `a -> d -> e -> f -> g -> b -> c`
    * `a -> b -> d -> e -> f -> g -> c`

Time to get them "evaluated":

Note:
* `x11` stands for `x1` in first lambda.
* `x12` stands for `x2` in first lambda.
* `x21` stands for `x1` in second lambda.
* `x22` stands for `x2` in second lambda.
* `x23` stands for `x3` in second lambda.

* `a -> b -> c -> d -> e -> f -> g`
    * `x11=10,x12=10`
    * `x=100` after `c` is done
    * `x21=100,x22=100,x23=100`
    * `x=100*100*100=1000000` after `g` is done
    * finally `x=1000000`
* `a -> b -> d -> c -> e -> f -> g`
    * `x11=10,x12=10,x21=10`
    * `x=100` after `c` is done
    * `x22=100,x23=100`
    * `x=10*100*100=100000` after `g` is done 
    * finally `x=100000`
* `a -> b -> d -> e -> c -> f -> g`
    * `x11=10,x12=10,x21=10,x22=10`
    * `x=100` after `c` is done
    * `x23=100`
    * `x=10*10*100=10000` after `g` is done 
    * finally `x=10000`
* `a -> b -> d -> e -> f -> c -> g`
    * `x11=10,x12=10,x21=10,x22=10,x23=10`
    * `x=100` after `c` is done
    * `x=10*10*10=1000` after `g` is done
    * finally `x=1000`
* `d -> e -> f -> g -> a -> b -> c`
    * `x21=10,x22=10,x23=10`
    * `x=1000` after `g` is done
    * `x11=1000,x12=1000`
    * `x=1000*1000` after `c` is done
    * finally `x=1000000`
* `a -> d -> e -> f -> g -> b -> c`
    * `x11=10,x21=10,x22=10,x23=10`
    * `x=1000` after `g` is done
    * `x12=1000`
    * `x=10*1000=10000` after `c` is done
    * finally `x=10000`
* `a -> b -> d -> e -> f -> g -> c`
    * `x11=10,x12=10,x21=10,x22=10,x23=10`
    * `x=1000` after `g` is done
    * `x=100` after `c` is done
    * finally `x=100`

So all possible values are:
    [1000000, 100000, 10000, 1000, 100]

*Which of these possibilities remain if we instead
use serialized procedures*:
    (define x 10)
    (define s (make-serializer))
    (parallel-execute
      (s (lambda () (set! x (* x x))))
      (s (lambda () (set! x (* x x x)))))

There are only two possibilities:

* The first procedure get fully executed before the second one can start
    * `x=100` after the first procedure is done
    * `x=1000000` after the second procedure is done
    * finally `x=1000000`
* The second procedure get fully executed before the first one can start
    * `x=1000` after the second procedure is done
    * `x=1000000` after the first procedure is done
    * finally `x=1000000`

So there is only one possible value after everything is done: `x=1000000`.
