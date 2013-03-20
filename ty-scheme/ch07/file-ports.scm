(define out
  (lambda (what)
    (begin
      (display what)
      (newline))))

(newline)

; the s-expr below is identical
;     current-output-port is used if no port is given

(display 9)
(display 9 (current-output-port))
; 99

(newline)

; now we've known 'open-{input,output}-file' & 'close-{input,output}-port'
; let's try to play with them, and do some IO operations

(define infile (open-input-file "in.txt"))

(out (read-char infile))
; #\T

(out (read-line infile))
; "each"

; write a function to read all the rest content!
(define read-all
  (lambda (port)
    ; we keep a list for what we've read
    (let read-next (
      (cur-content '()))
      (define cur-ch (read-char port))
      (if (eof-object? cur-ch)
	(list->string (reverse cur-content))
	(read-next (cons cur-ch cur-content))))))
(out (read-all infile))
(close-input-port infile)

; try to write factorials to a file
(define factorial
  (lambda (n)
    (if (= n 0) 1
      (* n (factorial (- n 1))))))

(define outfile (open-output-file "factorial.txt"))

(let write-factorial (
  (i 0))
  (if (> i 20) 'ok
    (begin
      (display i outfile)
      (write-char #\space outfile)
      (display (factorial i) outfile)
      (newline outfile)
      (write-factorial (+ i 1)))))

(close-output-port outfile)
