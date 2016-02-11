(ns logical.day1-easy)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)
(use 'logical.day1-book)

(db-rel languageo x l)
(db-rel system x s)

(def facts1
  (-> facts
      ;; I don't have a clear idea about what should be categorized
      ;; as "systemo", so don't be mad at me if I've got something wrong xD
      (db-fact systemo :alan-turing :bombe)
      (db-fact systemo :grace-hopper :univac)
      (db-fact languageo :grace-hopper :cobol)
      (db-fact systemo :leslie-lamport :distributed-systems)
      (db-fact languageo :leslie-lamport :latex)
      (db-fact languageo :alonzo-church :lambda-calculus)
      (db-fact systemo :ada-lovelace :analytical-engine)
      (db-fact systemo :barbara-liskov :liskov-substitiion-principle)
      (db-fact languageo :john-mccarthy :lisp)
      ))

(defn day1-easy
  []
  (p "exercise 1")
  ;; whatever element present in both list are shown up as results
  (p (run* [q] (membero q [1 2 3]) (membero q [4 3 2])))
  ;; (2 3)
  ;; and duplicated elements are all shown
  (p (run* [q] (membero q [1 2 3]) (membero q [4 3 2 1 1])))
  ;; (1 1 2 3)
  ;; returns no result if nothing in common
  (run* [q] (membero q [1 2 3]) (membero q [4 5 6]))
  ;; ()

  (p "exercise 2")
  (p (run* [q] (appendo [1 2 3] [4 5 6] q)))
  ;; ((1 2 3 4 5 6))
  (p (run* [q] (appendo [1 2 3] q [1 2 3 4 5 6])))
  ;; ((4 5 6))
  ;; should be a failure
  (p (run* [q] (appendo q [1 2 3] [1 2 3 4 5 6])))
  ;; ()
  (p (run* [q] (appendo q [1 2 3] [1 2 3 1 2 3])))
  ;; ((1 2 3))
  )
