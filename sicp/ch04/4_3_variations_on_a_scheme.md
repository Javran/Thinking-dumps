# Nondeterministic computiing

Specify a list `l`, the return value of `(an-element-of l)` can
be any of the valid member in `l`, as we use `(require condition)`
to specify some constraints, the set of possible valid values would
be narrowed down.

Personally I think this is essentially the same as list comprehensions
found in some programming languages. One step further, in Haskell,
list comprehensions are just syntactic sugars for list monad, which
is called "nondeterministic monad", compare the code between
Scheme and Haskell:

```scheme
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (requrie (prime? (+ a b)))
    (list a b)))
```

```haskell
primeSumPair :: [Int] -> [Int] -> [(Int, Int)]
primeSumPair = do
    a <- list1
    b <- list2
    guard (isPrime a b)
    return (a,b)
```

* stream processing: the illusion of time, we use the stream
as if the values are already produced ahead of time.

* nondeterministic evaluation: the illustion that time branches,
we are exploring multiple worlds at the same time. some worlds
fail to meet some constraints and come to an end, others might
survive, indicating a possible solution to the constraints.
