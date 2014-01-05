; test stream:
;   0, +2, 0, -2, 0, +2, 0, -2, ...
; expected averaged stream: 
; [0], 1, 1, -1, -1, 1, 1, -1, ...
; expected zero-crossings:
; [0], 0, 0, -1,  0, 1, 0, -1, ...
(define test-stream
  (cons-stream
    0
    (cons-stream
      2
      (cons-stream
        0
        (cons-stream
          -2 test-stream)))))
