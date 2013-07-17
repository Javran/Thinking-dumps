(load "../common/utils.scm")

; * one record file for each division
; * keyed on employees' names
; * each employees' record is a set (I think it's actually a dict)
;   * according to the requirement, we should at least have these fields:
;     * address
;     * salary

; each record will be stored as an s-expr
;   and put into a single file for each division
;   so that the I/O and file parsing is omitted here to simplify the code
; I'll design 3 different data structures for 3 indenpendent divisions
;   and then implement procedures required to demonstrate the ability of
;   data-directed programming

; furthermore, I assume the name is unique to all the employees represented.
;   so we don't need to care about the name confliction 

; a. Implement for headquarters a get-record procedure that re-
; trieves a specified employee’s record from a specified personnel
; file. The procedure should be applicable to any division’s file.
; Explain how the individual divisions’ files should be structured.
; In particular, what type information must be supplied?

; answer:
; The file should at least have information indicating which division it describes
; But I think attaching tags to the file content might violate some parsing logic
; for existing programs. So I choose to attach the tag after files have been parsed into
; Scheme's data structures. Moreover, since each file stands for a specified division,
; we can inform the structure used by identifying which division the file is belonging to.

; we need the foundation of data-directed programming, i.e. `put` and `get`
; as well as `attach-tag`, `type-tag` and `contents`
(load "./4_3_data_directed_put_get.scm")
(define all-division-info
  (list
    (list 'division-a "./exercise_2_74_division_a_data.scm")
    (list 'division-b "./exercise_2_74_division_b_data.scm")
    (list 'division-c "./exercise_2_74_division_c_data.scm")))

(define all-division-data
  (map (lambda (division-info)
         (let ((division-tag (car division-info))
               (division-filename (cadr division-info)))
           (attach-tag
             division-tag
             (call-with-input-file division-filename read))))
       all-division-info))

(define (get-record name division-data)
  ((get 'get-record (type-tag division-data))
    name
    (contents division-data)))

(load "./exercise_2_74_division_a_impl.scm")

(out (get-record "James" (car all-division-data)))

(end-script)
