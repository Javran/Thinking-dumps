(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./data-directed.scm")

;; how to use "data-directed.scm":
;; copy the code from line "begin template" to
;; "end template"
;;
;; supported operations:
;;
;; * (set <key1> <key2> <val>)
;;   "<val>" is assumed never being #f
;; * (get <key1> <key2>)
;;   return #f if the value is not found
;; * (initialize!)
;;   initialize the table

;; begin template

(define set #f)
(define get #f)
(define initialize! #f)

(let* ((f-alist (data-directed-functions))
       (set1 (cadr (assoc 'set f-alist)))
       (get1 (cadr (assoc 'get f-alist)))
       (init1 (cadr (assoc 'init f-alist))))
  (set! set set1)
  (set! get get1)
  (set! initialize! init1))

;; end template

(initialize!)

(set 'a 'a "aa")
(set 'a 'b "ab")
(set 'c 'a "ca")
(set 'a 'b 'ab)

(do-test
 get
 (list
  (mat 'a 'a "aa")
  (mat 'a 'b 'ab)
  (mat 'c 'a "ca")
  (mat 'no 'such #f)))

(initialize!)

(do-test
 get
 (list
  (mat 'a 'a #f)
  (mat 'a 'b #f)
  (mat 'c 'a #f)
  (mat 'no 'such #f)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
