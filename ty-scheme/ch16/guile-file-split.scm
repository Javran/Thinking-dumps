#!/usr/bin/env guile
!#
; split file into chunks

(load "../common/guile/utils.scm")

(define print-help
  (lambda ()
    (out
      "split file into chunks"
      "arguments: <source file> <target file prefix> <size in byte>")))

(define handle-arguments
  (lambda ()
    (let* ((argv (program-arguments))
           (argv-vector (list->vector argv)))
      (if (= (vector-length argv-vector) 4)
        (begin
          ; argument length check ok
          (let ((source-file
                  (vector-ref argv-vector 1))
                (target-file-prefix
                  (vector-ref argv-vector 2))
                (limit-size
                  (string->number (vector-ref argv-vector 3))))
            (out
              "source file:"
              source-file
              "target file prefix"
              target-file-prefix
              "size limit in byte:"
              limit-size)
            (do-split source-file target-file-prefix limit-size))
          #t)
        (begin
          (print-help)
          #f)))))

; http://www.gnu.org/software/guile/manual/html_node/File-System.html#File-System
; delete a file only if it's empty
; return true if the file is removed
(define delete-file-if-empty
  (lambda (filename)
    (if (file-exists? filename)
      (if (= 0 (stat:size (stat filename)))
        (begin
          (delete-file filename)
          #t)
        #f)
      #f)))


(define do-split
  (lambda (source-file target-file-prefix limit-size)
    (call-with-input-file source-file
      (lambda (handle)
        (let loop ((chunk-num 1))
          (let* ((outfile (string-append
                           target-file-prefix
                           "."
                           (number->string chunk-num)))
                (rest-size (copy-file-with-limit handle outfile limit-size)))
            (cond
              ((not rest-size) ; end of file reached
               ; if it is not the first outfile
               (if (not (= chunk-num 1))
                 (delete-file-if-empty outfile)))
              ((= rest-size 0) ; keep going
               (loop (+ chunk-num 1))))))))))

; copy content in handle to the outfile, with limited size
; return the remaining size (i.e. allowed size minus actual copied size)
; if the end of file of handle reached, #f will be returned
(define copy-file-with-limit
  (lambda (handle outfile limit-size)
    (call-with-output-file outfile
      (lambda (out-handle)
        (let loop ((rest-size limit-size))
          (if (= 0 rest-size)
            ; limitation reached
            0
            ; elsewise
            (let ((ch (read-char handle)))
              (if (eof-object? ch)
                ; rest size is not consumed in this case
                #f
                ; else copy the char to the target
                (begin
                  (write-char ch out-handle)
                  (loop (- rest-size 1)))))))))))



(define entry
  (lambda ()
    (handle-arguments)))

(entry)
