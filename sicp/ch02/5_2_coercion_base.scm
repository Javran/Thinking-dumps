(load "./4_3_data_directed_put_get.scm")

(define coercion-proc-table nil)

(define (put-coercion op type item)
  (set! coercion-proc-table
    (put-proc op type item coercion-proc-table)))

(define (get-coercion op type)
  (get-proc op type coercion-proc-table))

