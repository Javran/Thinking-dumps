;; syntax of rules
;; rules are of the form:
;; '(rule <conclusion> [rule-body])
(define rule?
  (list-tagged-with 'rule))
(define conclusion cadr)
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

;; an assertion to be added is a list
;; which begins with symbol "assert!"
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

;; a simple optimization:
;; if the patten begins with a constant symbol,
;; we might only need to search assertions with the same index.
;; Otherwise, if the pattern does not satisfy this criteria,
;; we instead return all the assertions from the system.
;; Further optimization is still possible.

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

;; get a stream, return an empty stream if not found
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (or s the-empty-stream)))

;; the following functions can't be purely tested.
;; TODO tests in qeval-tests

;; a big stream of all the assertions
(define THE-ASSERTIONS the-empty-stream)

;; "frame" is not use in the body of "fetch-assertions"
;; not sure what's the point of having this parameter.
(define (fetch-assertions pattern frame)
  (define (get-all-assertions) THE-ASSERTIONS)
  ;; assertion is indexed in a way that
  ;; the first key is the constant symbol,
  ;; and the second key is the symbol "assertion-frame"
  (define (get-indexed-assertions pattern)
    (get-stream (index-key-of pattern) 'assertion-stream))
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

;; rules are also big streams
(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (define (get-all-rules) THE-RULES)
  (define (get-indexed-rules pattern)
    (stream-append
     ;; different from assertions, rules might have variables
     ;; in conclusion, we put all possible rules together to form another stream
     (get-stream (index-key-of pattern) 'rule-stream)
     (get-stream '? 'rule-stream)))
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

;; add rules or assertions to database
(define (add-rule-or-assertion! assertion)
  ;; TODO
  ;; more compact, but I think more detailed explanation should be written here
  (define (indexable? pat)
    (or (constant-symbol? (car pat))
        (var? (car pat))))
  (define (add-assertion! assertion)
    (define (store-assertion-in-index assertion)
      (if (indexable? assertion)
          (let ((key (index-key-of assertion)))
            (let ((current-assertion-stream
                   (get-stream key 'assertion-stream)))
              (put key
                   'assertion-stream
                   (cons-stream
                    assertion
                    current-assertion-stream))))))

    (store-assertion-in-index assertion)
    (let ((old-assertions THE-ASSERTIONS))
      (set! THE-ASSERTIONS
            (cons-stream assertion old-assertions))
      'ok))

  (define (add-rule! rule)
    (define (store-rule-in-index rule)
      (let ((pattern (conclusion rule)))
        (if (indexable? pattern)
            (let ((key (index-key-of pattern)))
              (let ((current-rule-stream
                     (get-stream key 'rule-stream)))
                (put key
                     'rule-stream
                     (cons-stream rule
                                  current-rule-stream)))))))
    (store-rule-in-index rule)
    (let ((old-rules THE-RULES))
      (set! THE-RULES (cons-stream rule old-rules))
      'ok))
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (qeval-database-tests)
  ;; test rule predicate and accessors
  (do-test
   rule?
   (list
    (mat 'not-a-rule #f)
    (mat '(not-a-rule) #f)
    ;; rule without body
    (mat '(rule conclusion) #t)
    ;; rule with body
    (mat '(rule c (a b)) #t)))

  (do-test
   conclusion
   (list
    (mat '(rule concl) 'concl)
    (mat '(rule c (a b)) 'c)))

  (do-test
   rule-body
   (list
    ;; the rule is always true if its body
    ;; isn't present
    (mat '(rule concl) '(always-true))
    (mat '(rule c (a b)) '(a b))))

  ;; assertion predicate and accessor
  (do-test
   assertion-to-be-added?
   (list
    (mat '(assert! (fruit orange)) #t)
    (mat '(assert! (rule whatever)) #t)
    (mat '(query!) #f)))

  ;; assertion accessor
  (do-test
   add-assertion-body
   (list
    (mat '(assert! (fruit orange)) '(fruit orange))
    (mat '(assert! (rule whatever)) '(rule whatever))))

  'ok)

(if *qeval-tests*
    (qeval-database-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
