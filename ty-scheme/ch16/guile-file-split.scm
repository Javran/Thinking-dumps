#!/usr/bin/env guile
!#
; split file into chunks

(load "../common/guile/utils.scm")

(define print-help
  (lambda ()
    (out
      "split file into chunks"
      "arguments: <source file> <target file suffix> <size in byte>")))

(define handle-arguments
  (lambda ()
    (let* ((argv (program-arguments))
           (argv-vector (list->vector argv)))
      (if (= (vector-length argv-vector) 4)
        (begin
          ; argument length check ok
          (let ((source-file
                  (vector-ref argv-vector 1))
                (target-file-suffix
                  (vector-ref argv-vector 2))
                (limit-size
                  (string->number (vector-ref argv-vector 3))))
            (out
              "source file:"
              source-file
              "target file suffix:"
              target-file-suffix
              "size limit in byte:"
              limit-size)
            (do-split source-file target-file-suffix limit-size))
          #t)
        (begin
          (print-help)
          #f)))))

(define do-split
  (lambda (source-file target-file-suffix limit-size)
    ; TODO
    (out "working ...")))

(define entry
  (lambda ()
    (handle-arguments)))

(entry)
