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
  ;; backward on whicho1
  (p
   (run* [q]
     (fresh [s1 s2]
       (== s1 [:a :b :c])
       (== s2 [:b :c :d])
       (whicho1 q s1 s2 :both))))
  ;; backward on whicho2
  (p
   (run* [q]
     (fresh [s1 s2]
       (== s1 [:a :b :c])
       (== s2 [:b :c :d])
       (whicho2 q s1 s2 :both))))
  ;; it turns out both "whicho1" and "whicho2"
  ;; does not consider the fact of membership
  ;; in the other set. in other words
  ;; if (whicho1 x s1 s2 :two) is true,
  ;; it does not suggest "s1" does not contain "x"
  ;; so the result is not a set difference
  (p
   (run* [q]
     (fresh [s1 s2]
       (== s1 [:a :b :c])
       (== s2 [:b :c :d])
       (whicho1 q s1 s2 :two))))
  (p
   (run* [q]
     (fresh [s1 s2]
       (== s1 [:a :b :c])
       (== s2 [:b :c :d])
       (whicho2 q s1 s2 :two))))
  )
