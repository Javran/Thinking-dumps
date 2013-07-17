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
  (let ((tag (type-tag division-data))
        (data (contents division-data)))
    (let ((result ((get 'get-record tag) name data)))
      (if result
        (attach-tag tag result)
        #f))))

(load "./exercise_2_74_division_a_impl.scm")
(load "./exercise_2_74_division_b_impl.scm")
(load "./exercise_2_74_division_c_impl.scm")

; test get-record from 3 divisions
(out (get-record "James" (car all-division-data)))
(out (get-record "Susan" (cadr all-division-data)))
(out (get-record "Lisa" (caddr all-division-data)))

; b. Implement for headquarters a get-salary procedure that
; returns the salary information from a given employee’s record
; from any division’s personnel file. How should the record be
; structured in order to make this operation work?

; answer: 
; The record should at least contain information of the division
; it is belonging to. So I make the returned data from `get-record` tagged

(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

(newline)
(out (get-salary (get-record "James" (car all-division-data))))
(out (get-salary (get-record "Susan" (cadr all-division-data))))
(out (get-salary (get-record "Lisa" (caddr all-division-data))))
 
; c. Implement for headquarters a find-employee-record proce-
; dure. This should search all the divisions’ files for the record of
; a given employee and return the record. Assume that this pro-
; cedure takes as arguments an employee’s name and a list of all
; the divisions’ files.

(define (find-employee-record name division-list)
  (if (null? division-list)
    #f
    (let ((record (get-record name (car division-list))))
      (if record
        record
        (find-employee-record name (cdr division-list))))))

; make a list for all employees listed above, including their divisions and their salary information
(define test-employees
  (list "Susan" "James" "Donald" "NoSuch" "Mary" "Mark" "Jeff" "Betty"))

(newline)
(for-each
  (lambda (name)
    (let ((result (find-employee-record name all-division-data)))
      (if result
        (begin
          (display name)
          (display ":\t")
          (display (type-tag result))
          (display "\t")
          (display (get-salary result))
          (newline))
        (begin
          (display name)
          (display ":\t <not found>")
          (newline)))))
  test-employees)
 
; d. When Insatiable takes over a new company, what changes must
; be made in order to incorporate the new personnel information
; into the central system?

; answer:
; * implement `get-record` and `get-salary` accordingly
; * assign the division an identity (used as tag)
; * install new implementation into the system

(end-script)
