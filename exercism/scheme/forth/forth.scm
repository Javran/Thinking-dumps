(import (rnrs))

(use-modules
 ((srfi srfi-1)
  #:select (append-map span)))
(use-modules (ice-9 match))

(define (tokenize line)
  (string-split line #\space))

(define-record-type word-def
  (fields name body))

(define-record-type forth-state
  (fields
   (mutable stack)
   (mutable env)))

(define-record-type forth-closure
  (fields env body))

(define (forth-state-pop! state)
  (match (forth-state-stack state)
         [(hd . tl)
          (forth-state-stack-set! state tl)
          hd]))

(define (forth-state-push! state x)
  (forth-state-stack-set!
   state
   (cons x (forth-state-stack state))))

;; An env is an assoc with values being one of:
;; - procedure? that takes forth-state? as argument and mutates it.
;; - forth-closure? that stores user-defined words.
(define (make-initial-env)
  (let ([;; shorthand for defining functions that pop two elements.
         bin-op
         (lambda (f)
           (lambda (state)
             (let* ([b (forth-state-pop! state)]
                    [a (forth-state-pop! state)])
               ;; note that b and a are reversed
               ;; to make it looks like how stack is organized.
               (f b a state))))])
    (list
     (cons '+ (bin-op
               (lambda (b a state)
                 (forth-state-push! state (+ a b)))))
     (cons '- (bin-op
               (lambda (b a state)
                 (forth-state-push! state (- a b)))))
     (cons '* (bin-op
               (lambda (b a state)
                 (forth-state-push! state (* a b)))))
     (cons '/ (bin-op
               (lambda (b a state)
                 (forth-state-push! state (quotient a b)))))
     (cons 'drop forth-state-pop!)
     (cons 'dup (lambda (state)
                  (let ([a (forth-state-pop! state)])
                    (forth-state-push! state a)
                    (forth-state-push! state a))))
     (cons 'swap (bin-op
               (lambda (b a state)
                 (forth-state-push! state b)
                 (forth-state-push! state a))))
     (cons 'over (bin-op
               (lambda (b a state)
                 (forth-state-push! state a)
                 (forth-state-push! state b)
                 (forth-state-push! state a)))))))

;; Parses list of tokens.
(define (parse tokens)
  (match tokens
         [() '()]
         [(":") (raise 'incomplete-word-def)]
         [(":" name . rest)
          (if (string->number name)
              (raise 'cannot-redefine-numbers)
              (call-with-values
                  (lambda () (span (lambda (x) (not (string=? x ";"))) rest))
                (lambda (body rs)
                  (match rs
                         [(";" . after)
                          (cons
                           (make-word-def
                            (string->symbol (string-downcase name))
                            (parse body))
                           (parse after))]
                         [_ (raise 'incomplete-word-def)]))))]
         [(hd . tl)
          (let* ([hd-as-num (string->number hd)])
            (if hd-as-num
                (cons hd-as-num (parse tl))
                (cons
                 (string->symbol (string-downcase hd))
                 (parse tl))))]))

(define (perform-action! state action)
  (match action
         [(? procedure?) (action state)]
         [;; I'm not exactly sure why there is a #f in the ignored position...
          ($ forth-closure _ env body)
            (let ([;; make a new forth-state for closure to be executed in isolated environment.
                   clo-state
                   (make-forth-state
                    (forth-state-stack state)
                    env)])
              (interpret-all clo-state body)
              ;; while it's possible the one might define words inside a word definition,
              ;; in which case env in clo-state might be updated,
              ;; but test suite doesn't specify what to do in that case.
              ;; here I choose to simply ignore (potentially updated) env from clo-state.
              (forth-state-stack-set! state (forth-state-stack clo-state)))]))

(define (interpret state stmt)
  (match stmt
         [(? number?) (forth-state-push! state stmt)]
         [(? symbol?)
          (perform-action! state (cdr (assoc stmt (forth-state-env state))))]
         [($ word-def _ name body)
          (let* ([cur-env (forth-state-env state)]
                 [clo (make-forth-closure cur-env body)])
            (forth-state-env-set! state (cons (cons name clo) cur-env)))]
         [_ (raise 'unknown-statement)]))

(define (interpret-all state stmts)
  (for-each (lambda (stmt) (interpret state stmt)) stmts))

(define (forth program)
  (let ([stmts (parse (append-map tokenize program))]
        [state (make-forth-state '() (make-initial-env))])
    (interpret-all state stmts)
    (forth-state-stack state)))
