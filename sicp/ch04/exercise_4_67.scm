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
      (if (id-var? (binding-variable (car frame)))
          (let ((frame-vars (map binding-variable frame)))
            (let ((pred? (pred-of? (car frame-vars))))
              (let loop ((l 1)
                         (fr (cdr frame-vars)))
                (if (null? fr)
                    #f
                    (if (pred? (car fr))
                        l
                        (loop (add1 l) (cdr fr)))))))
          #f)))

(do-test
 find-binding-group-len
 (list
  (mat '( ((? 2 x) . a) ((? 1 x) . a)) 1)
  (mat '( ((? 2 x) . a) ((? 2 z) . b) ((? 2 t) . c)
          ((? 1 x) . a) ((? 1 z) . b) ((? 1 t) . c)) 3)
  (mat '() #f)
  (mat '( ((? 2 x) . a) ((? 2 z) . b) ) #f)
  ))

(define (looping-frame? frame)
  ;; decrease id in group1
  ;; to make it looks exactly like group2 if possible
  (define (tree-walk data)
    (cond ((id-var? data)
           `(? ,(sub1 (cadr data)) ,(caddr data)))
          ((pair? data)
           (cons (tree-walk (car data))
                 (tree-walk (cdr data))))
          (else data)))

  (let ((group-len (find-binding-group-len frame)))
    (if group-len
        (if (> (* 2 group-len) (length frame))
            #f
            (let ((group1 (sublist frame 0 group-len))
                  (group2 (sublist frame group-len (* 2 group-len))))
              (equal? (tree-walk group1) group2)
              ))
        #f)))

;; modify the existing impl:
;; use pattern matching to find valid frames
(define (simple-query-mod query-pattern frame-stream)
  (stream-intermap
   (lambda (frame)
     (if (looping-frame? frame)
         (error "an infinite loop detected, aborting...")
         'ok)
     (stream-append-delayed
      ;; search against assertions
      (find-assertions query-pattern frame)
      ;; search against rules
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define simple-query simple-query-mod)
(assert-error
 (lambda ()
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

   (out (qe-all '(link a d)))))

;; from: 4_4_1_rules.scm
;; "outranked-by" modified according to exercise 4.64.
(load "./4_4_1_deductive_information_retrieval.scm")

(define simple-query simple-query-mod)

(assert-error
 (lambda ()
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

   (qe-all '(outranked-by (Bitdiddle Ben) ?who))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
