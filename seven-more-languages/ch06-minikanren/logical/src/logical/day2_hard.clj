(ns logical.day2-hard)

(use 'clojure.core.logic)
(use 'logical.utils)

(defne insideo [e l]
  ([_ [e . _]])
  ([_ [_ . t]]
   (insideo e t)))

;; the problem with "insideo" is:
;; when "e" matches the head of "l", it goes to both branches
;; and in second branch we cannot tell whether the current head element
;; is a duplicated one (because that info is on "another universe")
;; our solution is to keep track of this information in the second branch
;; one way to do this is to use constraint "!=" (let's call it "insideo1")
;; when we run "insideo1" backwards, it goes to both branch, and at the same
;; time, one branch has the fact that e == h while another e != h.
;; as we traverse through the list, the second branch will accumulate
;; "!=" constraints, this has the effect of eliminating duplicated elements
;; let's make an example to see how this works:
;; say we are running "(insideo1 q [:a :b :a])":
;; * first branch succeeds, with solution that "q == :a"
;; * second branch succeeds, with constraint that "q != :a"
;;   * recursively, first branch succeeds with "q != :a and q == :b"
;;   * second branch succeeds with constraint that "q != a and q != b"
;;     * recursively, first branch fails because we already know "q != a"
;;     * second branch succeeds, with constraints still being "q != a and q != b"
;;       * recursively, no branch will match the empty list, and the query fails.

;; the following is the first working impl,
;; might be easier to read than the simplified one
;; so I keep it here.
#_
(defne insideo1 [e l]
  ([_ [h . t]]
   (conde
    [(== e h)]
    [(!= e h) (insideo1 e t)]
   )))
(defne insideo1 [e l]
  ([_ [e . _]])
  ([_ [h . t]]
   (!= h e)
   (insideo e t)))

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
