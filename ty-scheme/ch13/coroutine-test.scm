(load "../common/utils.scm")
(load "../common/defstruct.scm")
(load "../common/coroutines.scm")

(define make-matcher-coroutine
  ; accepts two tree-gen-coroutines
  (lambda (tree-cor-1 tree-cor-2)
    ; don't need initial argument
    (coroutine place-holder
      ; coroutine body
      (let loop ()
        ; resume two tree coroutines to get 2 leaves
        (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
              (leaf2 (resume tree-cor-2 'get-a-leaf)))
          (if (eqv? leaf1 leaf2)
            (if (null? leaf1)
              ; then
              ; if the leaf is an empty list, the matching is done
              #t
              ; else
              (loop))
            #f))))))

(define make-leaf-gen-coroutine
  (lambda (tree matcher-cor)
    (coroutine place-holder
      (let loop ((tree tree))
        (cond
          ((null? tree) 'skip)
          ((pair? tree)
              (loop (car tree))
              (loop (cdr tree)))
          (else
            (resume matcher-cor tree))))
      (resume matcher-cor '()))))

; here we've made two leaf-gen-coroutine and a matcher-coroutine
(define same-fringe?
  (lambda (tree1 tree2)
    (letrec ((tree-cor-1
               (make-leaf-gen-coroutine
                 tree1
                 ; wrap matcher-cor inside lambda
                 (lambda (v) (matcher-cor v))))
             (tree-cor-2
               (make-leaf-gen-coroutine
                 tree2
                 (lambda (v) (matcher-cor v))))
             (matcher-cor
               (make-matcher-coroutine
                 (lambda (v) (tree-cor-1 v))
                 (lambda (v) (tree-cor-2 v)))))
      (matcher-cor 'start-ball-rolling))))

(out (same-fringe? '(1 2 (3 4 5)) '((1 2) 3 (4 5))))
; #t
(out (same-fringe? '(1 2 (3 4 5)) '((1 2) 4 (3 5))))
; #f

; TODO: what's happening here?
