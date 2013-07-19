(load "../common/utils.scm")

; basic supports from previous codes
(load "./4_3_data_directed_put_get.scm")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

(end-script)
