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
           []
           (with-db story-db
             (run* [q]
               (fresh [m1 m2 g]
                 ;; we have 2 distinct murderers
                 (!= m1 m2)
                 (membero m1 all-possible-murderers)
                 (membero m2 all-possible-murderers)
                 ;; and make goals using them
                 ;; note that this goal is too restrictive
                 ;; to make a story that contains more than 2 murders
                 ;; because as long as all goals are satisfied,
                 ;; the story generation stops.
                 ;; it would be better to have "goal" in a less restrictive form:
                 ;; to say that m1 and m2 are members of this goal
                 ;; but the goal is free to include any extra thing,
                 ;; the code would look like:
                 ;; (membero m1 g)
                 ;; (membero m2 g)
                 ;; (storyo* (shuffle start-state1) g q)
                 ;; but this doesn't work, because Clojure have no idea
                 ;; about what type of "g" we are using.
                 ;; for now let's just live with the fact
                 ;; that most of the time this story generator only
                 ;; generates stories with exactly 2 murders.
                 ;; this at least satisfies what exercise requies us to do.
                 (storyo* (shuffle start-state1) [m1 m2] q)))))]
    (print-story
     (first
      (filter #(> (count %) 4)
              (story-stream))))))
