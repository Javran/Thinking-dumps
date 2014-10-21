;; add breakpoint to bp-table
(define (breakpoint-table-add lbl offset tbl)
  (let ((offsets
         (let ((pair (assoc lbl tbl)))
           (if pair
               (cadr pair)
               '()))))
    `((,lbl (,offset ,@offsets))
      ,@(del-assoc lbl tbl))))

;; del breakpoint from bp-table
(define (breakpoint-table-del lbl offset tbl)
  (let ((result (assoc lbl tbl)))
    (if result
        `((,lbl ,(delete offset (cadr result)))
          ,@(del-assoc lbl tbl))
        ;; label not found, nothing to be done
        tbl)))

;; test if a label with an offset is registered in the table
(define (breakpoint-table-check? lbl offset tbl)
  (let ((result (assoc lbl tbl)))
    (and result (member offset (cadr result)))))
