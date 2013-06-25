(load "../common/utils.scm")

(define (make-tree entry left right)
  (list entry left right))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
    #f
    (let ((current-entry (entry set-of-records)))
      (cond ((= given-key (key current-entry))
             current-entry)
            ((< given-key (key current-entry))
             (lookup given-key (left-branch set-of-records)))
            ((> given-key (key current-entry))
             (lookup given-key (right-branch set-of-records)))))))

(define make-entry cons)
(define key car)
(define value cdr)

(define one-to-seven-tree
  (make-tree (make-entry 4 'four)
             (make-tree (make-entry 2 'two)
                        (make-tree (make-entry 1 'one)
                                   nil
                                   nil)
                        (make-tree (make-entry 3 'three)
                                   nil
                                   nil))
             (make-tree (make-entry 6 'six)
                        (make-tree (make-entry 5 'five)
                                   nil
                                   nil)
                        (make-tree (make-entry 7 'seven)
                                   nil
                                   nil))))

(for-each 
  (lambda (result)
    (if result
      (out result)
      (out "<No result>")))
  (map (lambda (x)
         (lookup x one-to-seven-tree))
       (list-in-range 1 10)))

(end-script)
