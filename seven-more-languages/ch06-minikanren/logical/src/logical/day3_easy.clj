(ns logical.day3-easy)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.day3-book)
(use 'logical.utils)

(require '[clojure.core.logic.fd :as fd])

(defn day3-easy
  []
  (p "day 3 - do easy")
  ;; let's find pythagorean triples
  (p
   (run* [a b c]
     (fd/in a b c (fd/interval 1 100))
     (fd/<= a b)
     (fd/<= b c)
     (fd/eq
      (= (+ (* a a) (* b b)) (* c c)))))
  ;; some examples to see "conde" working in action
  ;; every point within [0:10,0:10] that satisfies
  ;; either x + y == 10 or x == y
  (p
   (run* [a b]
     (fd/in a b (fd/interval 0 10))
     (conde
      [(fd/eq
        (= a b))]
      [(fd/eq
        (= (+ a b) 10))])))

  (let* [start-state1
         ;; there are 2 ways to make the motorist never appear:
         ;; either we put some extra rules to rule out the case
         ;; in which :maybe-motorist appears
         ;; or we can just simply remove this resource from start state.
         ;; here we go with the second approach
         [:maybe-telegram-girl
          :wadsworth :mr-boddy :cook :yvette]
         ;; get a list of all possible murders from story-elements
         all-possible-murderers
         (distinct
          (filter
           #(.startsWith (name %) "guilty-")
           (map #(get % 1) story-elements)))
         story-stream
         (fn
           [& goals]
           (with-db story-db
             (run* [q]
               (storyo* (shuffle start-state1) (vec goals) q))))]
    #_
    (print-story
     (first
      (filter #(> (count %) 5)
              (story-stream :guilty-peacock :dead-yvette))))
    (p all-possible-murderers)

    )
  )
