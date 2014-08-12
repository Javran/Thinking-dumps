;; Let's try to create a better machine simulator
;; and name it "simu"

(load "./data-directed.scm")

(define set-handler #f)
(define get-handler #f)
(define init-handler-table! #f)

(let* ((f-alist (global-table-functions))
       (set1 (cadr (assoc 'set f-alist)))
       (get1 (cadr (assoc 'get f-alist)))
       (init1 (cadr (assoc 'init f-alist))))
  (set! set-handler set1)
  (set! get-handler get1)
  (set! init-handler-table! init1))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  ;; TODO: make alists instead of passing these arguments
  ;; which is error prone.
  ;; since we only run this once for each instruction,
  ;; I think the performance won't be an issue
  'todo)

;; Local variables:
;; proc-entry: ""
;; End:
