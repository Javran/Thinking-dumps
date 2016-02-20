(ns logical.day2-easy)

(use 'clojure.core.logic)
(use 'logical.utils)

(defne extendo2 [xs ys rs]
  ([[] ys ys])
  ([[hd . tl] ys [hd . rs1]]
   (extendo2 tl ys rs1)))

(defn non-rooto
  [m]
  (fresh [u]
    (featurec m {:user u})
    (!= u "root")))

(defn whicho1 [x s1 s2 r]
  (conde
   [(membero x s1)
    (== r :one)]
   [(membero x s2)
    (== r :two)]
   [(membero x s1)
    (membero x s2)
    (== r :both)]))

(defn whicho2 [x s1 s2 r]
  (conda
   [(all
     (membero x s1)
     (membero x s2)
     (== r :both))]
   [(all
     (membero x s1)
     (== r :one))]
   [(all
     (membero x s2)
     (== r :two))]
   ))

(defne not-membero [e l]
  ([_ [v . rs]]
   (!= e v) (not-membero e rs))
  ([_ []]))

(defn whichxo1 [x s1 s2 r]
  (conde
   [(membero x s1)
    (== r :one)]
   [(membero x s2)
    (== r :two)]
   [(membero x s1)
    (membero x s2)
    (== r :both)]
   [(not-membero x s1)
    (not-membero x s2)
    (== r :none)]))

(defn whichxo2 [x s1 s2 r]
  (conda
   [(all
     (membero x s1)
     (membero x s2)
     (== r :both))]
   [(all
     (membero x s1)
     (== r :one))]
   [(all
     (membero x s2)
     (== r :two))]
   [(all
     (not-membero x s1)
     (not-membero x s2)
     (== r :none))]))

(defn day2-easy
  []
  (p "day 2 - do easy")
  (p "exercise 1")
  (p
   (run* [q]
     (extendo2 [1 2 3] [4 5 6] q)))
  (p
   (run* [q]
     (extendo2 q [4 5 6] [1 2 3 4 5 6])))
  (p
   (run* [q]
     (extendo2 [1 2 3] q [1 2 3 4 5 6])))
  (p "exercise 2")
  ;; no result because the query will fail
  (p
   (run* [q]
     (fresh [m]
       (== m {:user "root"})
       (non-rooto m))))
  ;; gets one result because the query will succeed
  (p
   (run* [q]
     (fresh [m]
       (== m {:user "user"})
       (non-rooto m))))
  (p "exercise 3")
  ;; we have two "whicho"s to use, one built with
  ;; conde and another conda
  (defn test-whicho
    [whicho s1 s2 r]
    (run* [q]
      (whicho q s1 s2 r)))
  ;; backward on whicho1
  (p
   (test-whicho
    whicho1
    [:a :b :c] [:b :c :d] :both))
  ;; backward on whicho2
  (p
   (test-whicho
    whicho2
    [:a :b :c] [:b :c :d] :both))
  ;; it turns out both "whicho1" and "whicho2"
  ;; does not consider the fact of membership
  ;; in the other set. in other words
  ;; if (whicho1 x s1 s2 :two) is true,
  ;; it does not suggest that "s1" does not contain "x"
  ;; so the result is not a set difference
  (p
   (test-whicho
    whicho1
    [:a :b :c] [:b :c :d] :two))
  (p
   (test-whicho
    whicho2
    [:a :b :c] [:b :c :d] :two))
  ;; it seems "conde" and "conda" is not making any
  ;; difference when being ran backwards, this is because
  ;; we have specified the result to be one of :one, :two or :both
  ;; which is mutually exclusive to each other.
  ;; Therefore in both "conde" and "conda", the result has selected exactly
  ;; one branch.
  (p "exercise 4")
  ;; we first need a relation "not-membero"
  ;; because in Core.logic it doesn't seem to have a way
  ;; of negation in general
  (p (run* [q] (not-membero 4 [])))
  (p (run* [q] (not-membero 1 [1 2 3])))
  (p (run* [q] (not-membero 4 [1 2 3])))

  ;; "whichxo" is like "whicho" but additionally
  ;; has :none branch
  (p (run* [q]
       (whichxo1 :a [1 2 3] [4 5 6] q)))
  (p (run* [q]
       (whichxo2 :a [1 2 3] [4 5 6] q)))
  ;; two programs above should give same results

  ;; specify :none, run it backwards and see what will happen
  (p (run* [q]
       (whichxo1 q [1 2 3] [4 5 6] :none)))
  (p (run* [q]
       (whichxo2 q [1 2 3] [4 5 6] :none)))
  ;; two programs above all seem to generate a list of things
  ;; "q" should not equal to

  ;; what if two sets have something in common?
  (p (run* [q]
       (whichxo1 q [1 2 3] [2 3 6] :none)))
  (p (run* [q]
       (whichxo2 q [1 2 3] [2 3 6] :none)))
  ;; two programs produce same result. as we know this logic
  ;; system adds conditions only when necessary, the result
  ;; looks "simplified" in a sense that no duplicated condition is added
  ;; even if some element appears in more than one set.
  ;; (e.g. only one "(!= ? 2)" condition despite that all two sets contain 2)
  )
