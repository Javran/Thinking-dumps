;; update-insts! will preserve the order of all the labels
;; despite there are multiple labels with exactly the same name,
;; there are all kept in the original implementation.
;; however when looking up the table, the first binding wins.
;; `a` will be 3 when reaching the label `there`

;; guess we would have some problem here
;; using the most naive approach below, we will be warned when
;; the second duplicated label counting from the last instruction
;; to the first one is found.
;; what we usually expect is receiving a warning when the second duplicated
;; label from first instruction to the last one is found.
;; TODO: having an extra pass to solve this problem:
;;   extracting labels only and test if every label is unique
;;   therefore we can scan from the beginning to the end
;;   I don't actually care about the performance
;;   as we only need to assemble it once, the "simulation" performance
;;   should be more important than this.
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       ;; note this procedure gets executed
       ;; *after* this inner recursive call to "extract-labels" is done
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (do
                   (if (assoc next-inst insts)
                       (error "label not unique: " next-inst)
                       'ok)
                   (receive insts
                       (cons (make-label-entry
                              next-inst
                              insts)
                             labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))
