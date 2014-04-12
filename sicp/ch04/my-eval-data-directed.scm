; infrastructure for data-directed style

(define my-eval-get #f)
(define my-eval-put! #f)
(define my-eval-test-installed-handlers #f)
(define my-eval-get-all-slot-names #f)

; definition:
;  slot: a symbol, the syntax structure name, the tag of the expression
;    e.g.: `'let` should be the corresponding slot name for
;           the handler that deals with let-expression
;  handler: handler structure,
;           please refer to "./my-eval-handler.scm"
;
; data structure:
; `eval-handler-alist` is an alist
;   key: a symbol the slot name
;   value: a handler, see definition part

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

  ; test a single slot from `alist`
  (define (test-slot slot alist)
    (format #t "Testing ~A " slot)
    (define handler
      (get-handler slot alist))
    (assert handler "handler not found")
    (define result
      (handler-run-test handler))
    (format #t "~%  Result: ~A~%" result)
    result)

  (define (test-all-slots)
    (define slots
      (map car eval-handler-alist))
    (newline)
    (define results
      (map (lambda (slot)
             (test-slot slot eval-handler-alist))
           slots))
    (define not-ok
      (map
        car
        (filter
          (lambda (pair)
            (not (eq? (cdr pair) 'ok)))
          (map cons slots results))))
    (out "Summary: slots that did not return with 'ok: ")
    (format #t "  ~A~%" not-ok)
    (out "Test done.")
    'ok)

  (set! my-eval-get get)
  (set! my-eval-put! put!)
  (set! my-eval-test-installed-handlers test-all-slots)
  (set! my-eval-get-all-slot-names
        (lambda ()
          (map car eval-handler-alist)))
  'ok)
