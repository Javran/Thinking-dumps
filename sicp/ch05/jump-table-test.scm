(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; a test for building up jump tables
;; the simplified list elements are either
;; symbol or a list of something (it doesn't matter
;; for our purpose)
;; let's try to do the two instruction list approach


(define (build-jump-table origin-insns)
  (let* ((label? symbol?)
         (collected
          ;; collect labels and instructions in order
          (let loop ((labels '())
                     (no-lbl-insns '())
                     (insns origin-insns))
            (if (null? insns)
                ;; because the elements are accumulated
                ;; from left to right, so we need to
                ;; reverse them when we are done
                ;; we are using "reverse!" to save space
                ;; and increase efficiency.
                ;; since we are not using the result anywhere
                ;; else, this destructive version is safe.
                (cons (reverse! labels)
                      (reverse! no-lbl-insns))
                (let ((hd (car insns))
                      (tl (cdr insns)))
                  (if (label? hd)
                      (loop (cons hd labels)
                            no-lbl-insns
                            tl)
                      (loop labels
                            (cons hd no-lbl-insns)
                            tl)))))))
    collected))


(for-each
 out
 (build-jump-table
  '(a
    (a b)
    b
    c
    d
    (c d e)
    (e)
    (f)
    g
    (h)
    (i j k)
    l
    (j k)
    m
    (m n)
    i)))

(end-script)
