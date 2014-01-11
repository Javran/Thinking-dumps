(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (list-of-values-from-left exps env)
  (if (no-opearnds? exps)
    '()
    ; I'm not sure if the value is guaranteed
    ;   to be evaluated immediately in `let`'s body
    ;   in order to be extremely safe
    (let ((left-result
            (eval (first-operand exps) env)))
      (begin
        ; force it
        left-result
        (let ((right-result
                (list-of-values-from-left
                  (rest-operands exps)
                  env)))
          (begin
            ; force it
            right-result
            (cons left-result
                  right-result)))))))

(define (list-of-values-from-right exps env)
  (if (no-opearnds? exps)
    '()
    (let ((right-result
            (list-of-values-from-right
              (rest-operands exps)
              env)))
      (begin
        ; force it
        right-result
        (let ((left-result
                (eval (first-operand exps) env)))
          (begin
            ; force it
            left-result
            (cons left-result
                  right-result)))))))

(end-script)
