(define GCD-first-version
  '(
    (data-paths
     (registers
      ((name a)
       (buttons ((name a<-b) (source (register b)))))
      ((name b)
       (buttons ((name b<-t) (source (register t)))))
      ((name t)
       (buttons ((name t<-r) (source (operation rem))))))
     (operations
      ((name rem) (inputs (register a) (register b)))
      ((name =) (inputs (registers b) (constant 0)))))
    (controller
     test-b
     (test =) ; isn't readable in my opinion
     ;; I personnally think "label" is not necessary
     ;; why not (branch gcd-done) ?
     (branch (label gcd-done)) ; conditional branch..
     (t<-r)
     (a<-b)
     (b<-t)
     (goto (label test-b))
     gcd-done)))

;; draw back: must constantly refer back to the
;; definition

(define GCD-second-version
  '(
    (controller
     test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

;; now it's easier to read
;; drawback: more verbose.
;; In the book it suggests it "look like Lisp expressions"
;; but I don't see why this is bad.. anyway.
