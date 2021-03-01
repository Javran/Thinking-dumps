(import (rnrs))

(use-modules ((srfi srfi-1)
              #:select (span)))

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
  (let ([st (forth-state-stack state)])
    (forth-state-stack-set! state (cdr st))
    (car st)))

(define (forth-state-push! state x)
  (forth-state-stack-set!
   state
   (cons x (forth-state-stack state))))

(define (make-initial-env)
  (let ([bin-op (lambda (f)
                  (lambda (state)
                    (let* ([b (forth-state-pop! state)]
                           [a (forth-state-pop! state)])
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
  (cond
   [(null? tokens) '()]
   [(list? tokens)
    (let* ([hd (car tokens)]
           [hd-as-num (string->number hd)]
           [tl (cdr tokens)])
      (cond
       [(string=? hd ":")
        (call-with-values
            (lambda () (span (lambda (x) (not (string=? x ";"))) tl))
          (lambda (ls rs)
            (if (and
                 (not (null? ls))
                 (not (null? rs))
                 (string=? (car rs) ";"))
                (if (string->number (car ls))
                    (raise 'cannot-redefine-numbers)
                    (cons
                     (make-word-def
                    (string->symbol (string-downcase (car ls)))
                    (parse (cdr ls)))
                     (parse (cdr rs))))
                (raise 'incomplete-word-def))))]
       [hd-as-num (cons hd-as-num (parse tl))]
       [else
        (cons
         (string->symbol (string-downcase hd))
         (parse tl))]))]))

(define (perform-action! state action)
  (cond
   [(procedure? action)
    (action state)]
   [(forth-closure? action)
    (let ([clo-state
           (make-forth-state
            (forth-state-stack state)
            (forth-closure-env action))])
      (interpret-all clo-state (forth-closure-body action))
      (forth-state-stack-set! state (forth-state-stack clo-state)))]))

(define (interpret state stmt)
  (cond
   [(number? stmt)
    (forth-state-push! state stmt)]
   [(symbol? stmt)
    (perform-action!
     state
     (cdr (assoc stmt (forth-state-env state))))]
   [(word-def? stmt)
    (let* ([cur-env (forth-state-env state)]
           [clo (make-forth-closure cur-env (word-def-body stmt))])
      (forth-state-env-set!
       state
       (cons (cons (word-def-name stmt) clo)
             cur-env)))]
    [else (raise 'unknown-statment)]))

(define (interpret-all state stmts)
  (for-each (lambda (stmt) (interpret state stmt)) stmts))

(define (forth program)
  (let* ([tokens (apply append (map tokenize program))]
         [stmts (parse tokens)]
         [state (make-forth-state '() (make-initial-env))])
    (interpret-all state stmts)
    (forth-state-stack state)))
