(ns logical.day2-hard)

(use 'clojure.core.logic)
(use 'logical.utils)

(defne insideo [e l]
  ([_ [e . _]])
  ([_ [_ . t]]
   (insideo e t)))

(defne insideo1 [e l]
  ([_ [h . t]]
   (conde
    [(== e h)]
    [(!= e h) (insideo1 e t)]
   )))

;; some reading regarding condX family:
;; http://stackoverflow.com/questions/10843563/conda-condi-conde-condu
(defn day2-hard
  []
  (p "day 2 - do hard")
  ;; this should succeed twice, because there are 2 :a
  ;; in the list that matches the first argument
  (p (run* [q]
       (insideo :a [:a :b :a])))

  ;; the following should succeed only once
  (p (run* [q]
       (insideo1 :a [:a :b :a])))

  ;; the following should return 4 distinct solutions
  (p (run* [q]
       (insideo1 q [:a :b :a :c :d :a])))
  )
