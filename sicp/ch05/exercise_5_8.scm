(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; NOTE: the following changes have been applied to legacy-easy and simu

(load "./legacy-easy.scm")
;; update-insts! will preserve the order of all the labels
;; despite there are multiple labels with exactly the same name,
;; there are all kept in the original implementation.
;; however when looking up the table, the first binding wins.
;; `a` will be 3 when reaching the label `there`

;; we solve this problem by having an extra pass:
;;   extracting labels only and test if every label is unique
;;   therefore we can scan from the beginning to the end
;;   I don't actually care about the performance
;;   as we only need to assemble it once, the "simulation" performance
;;   should be more important than this.

;; this one is quick-and-dirty:
;; using the most naive approach below, we will be warned when
;; the second duplicated label counting from the last instruction
;; to the first one is found.
;; what we usually expect is receiving a warning when the second duplicated
;; label from first instruction to the last one is found.
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       ;; note this procedure gets executed
       ;; *after* this inner recursive call to "extract-labels" is done
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (begin
                 (if (assoc next-inst labels)
                     (error "label not unique:" next-inst)
                     'ok)
                 (receive insts
                     (cons (make-label-entry
                            next-inst
                            insts)
                           labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(assert-error
 (lambda ()
   (let ((m (make-and-execute
             `(controller
               label-a
               (assign n (const 1))
               label-a
               (assign n (const 2))
               label-a
               (assign n (const 3)))
             '((n 10)))))
     (out (get-register-contents m 'n))))
 "mutiple labels with the same name will raise an error")

(load "./legacy-easy.scm")
(load "./exercise_5_8_common.scm")

;; some tests
(do-test
 first-dup-element
 (list
  (mat '() #f)
  (mat '(a b c d) #f)
  (mat '(a b a d) '(a b a d))
  (mat '(a b c d e c) '(c d e c))))

(define (extract-labels text receive)
  (scan-duplicate-labels text symbol?)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                   (cons (make-label-entry
                          next-inst
                          insts)
                         labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(assert-error
 (lambda ()
   (let ((m (make-and-execute
             `(controller
               label-a
               (assign n (const 1))
               label-a
               (assign n (const 2))
               label-a
               (assign n (const 3)))
             '((n 10)))))
     (out (get-register-contents m 'n))))
 "mutiple labels with the same name will raise an error")
