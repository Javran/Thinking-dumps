; infrastructure for data-directed style

(define my-eval-get #f)
(define my-eval-put! #f) 

; definition:
;  slot: a symbol, the syntax structure name, the tag of the expression
;    e.g.: `'let` should be the corresponding slot name for
;          the handler that deals with let-expression
; 
; data structure:
; `eval-handler-alist` is an alist
;   key: a symbol the slot name
;   value: a procedure of type: Exp x Env -> ExpVal
;          this procedure is supposed to evaluate the expression
;          and return the value of that expression

(let ((eval-handler-alist '()))
  ; keep the handlers inside

  ; modify `alist` to insert / replace
  ;   handler of `slot` with `handler`
  ; return: a pair with 
  ;   `car` set to the modified alist
  ;   `cdr` set to the old handler, or #f
  (define (put-handler slot handler alist)
    (let ((slot-list (assq slot alist)))
      (cons (cons (list slot handler)
                  (del-assq slot alist))
            (if slot-list
              ; fetch the old handler
              (cadr slot-list)
              ; or it does not exist
              #f))))

  ; put `handler` into the `slot` of `eval-handler-alist`
  ; return: the old handler, or #f
  (define (put! slot handler)
    (let ((modified
            (put-handler slot handler eval-handler-alist)))
      (set! eval-handler-alist (car modified))
      (out eval-handler-alist)
      (cdr modified)))

  ; look up `slot` in `alist`
  ; return: the corresponding handler, #f elsewise
  (define (get-handler slot alist)
    (let ((slot-list (assq slot alist)))
      (if slot-list
        ; fetch handler from list
        (cadr slot-list)
        #f)))

  ; look up `slot` in `eval-handler-alist`
  ; return: the same as `get-handler`
  (define (get slot)
    (get-handler slot eval-handler-alist))

  (set! my-eval-get get)
  (set! my-eval-put! put!)
  'done)
