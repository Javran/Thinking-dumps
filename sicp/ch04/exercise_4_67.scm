(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

;; test if the data is a variable with id
(define (id-var? data)
  (and (list? data)
       (= (length data) 3)
       (eq? (car data) '?)
       (number? (cadr data))))

;; generate a predicate to test if the data
;; is the predecessor of id-var
;; e.g. the predecessor of (? 2 a) is (? 1 a)
(define (pred-of? id-var)
  (lambda (data)
    (and (id-var? data)
         (eq? (caddr data) (caddr id-var))
         (= (cadr data) (sub1 (cadr id-var))))))

;; find binding group length
(define (find-binding-group-len frame)
  (if (null? frame)
      #f
      (let ((pred? (pred-of? (car frame))))
        (let loop ((l 1)
                   (fr (cdr frame)))
          (if (null? fr)
              #f
              (if (pred? (car fr))
                  l
                  (loop (add1 l) (cdr fr))))))))



#|
(apply
 qe-fresh-asserts!
 '(
   (edge a b)
   (edge b c)
   (edge c d)

   ;; wrong rule
   (rule (link ?x ?y) (edge ?x ?y))
   (rule (link ?x ?z)
         (and (link ?y ?z)
              (edge ?x ?y)))

   ;; correct one
   (rule (link2 ?x ?y) (edge ?x ?y))
   (rule (link2 ?x ?z)
         (and (edge ?x ?y)
              (link2 ?y ?z)))

   ))

(out (qe-all '(link a d)))

;; from: 4_4_1_rules.scm
;; "outranked-by" modified according to exercise 4.64.
(load "./4_4_1_deductive_information_retrieval.scm")

(apply
 qe-asserts!
 '(
   (rule (lives-near ?person-1 ?person-2)
         (and (address ?person-1 (?town . ?rest-1))
              (address ?person-2 (?town . ?rest-2))
              (not (same ?person-1 ?person-2))))

   (rule (same ?x ?x))

   (rule (wheel ?person)
         (and (supervisor ?middle-manager ?person)
              (supervisor ?x ?middle-manager)))

   (rule (outranked-by ?staff-person ?boss)
         (or (supervisor ?staff-person ?boss)
             (and (outranked-by ?middle-manager ?boss)
                  (supervisor ?staff-person ?middle-manager))))

   ))

(qe-all '(outranked-by (Bitdiddle Ben) ?who))

|#

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
