Only REPL interactions for now.

```clojure
user=> ;; exercise 1

user=> ;; prediction: whatever element present in both list are shown up as results

user=> (run* [q] (membero q [1 2 3]) (membero q [4 3 2]))
(2 3)
user=> ;; and duplicated elements are all shown

user=> (run* [q] (membero q [1 2 3]) (membero q [4 3 2 1 1]))
(1 1 2 3)
user=> ;; returns no result if nothing in common

user=> (run* [q] (membero q [1 2 3]) (membero q [4 5 6]))
()
user=> ;; exercise 2

user=> (run* [q] (appendo [1 2 3] [4 5 6] q)) ;; q = [1 2 3 4 5 6]?
((1 2 3 4 5 6))
user=> (run* [q] (appendo [1 2 3] q [1 2 3 4 5 6])) ;; q = [4 5 6]?
((4 5 6))
user=> (run* [q] (appendo q [1 2 3] [1 2 3 4 5 6])) ;; failure?
()
user=> (run* [q] (appendo q [1 2 3] [1 2 3 1 2 3])) ;; q = [1 2 3]
((1 2 3))
user=> ;; exercise 3

user=> ;; TODO: no idea, I need a concrete table of info
```
