(load "../common/utils.scm")

(define x (list 1 (list 2 (list 3 4))))
; (list 3 4) => (3 4)
; (list 2 (list 3 4) => (2 (3 4))
; (list 1 (list 2 (list 3 4))) => (1 (2 (3 4)))

; underlying structure:
(define y
  (cons 1
        (cons (cons 2
                    (cons (cons 3
                                (cons 4 nil))
                          nil))
              nil)))

; (error "comment this line to see the result")

(out (equal? x y))
(out x)

; TODO: interpretation?

(end-script)
