#lang racket

(require srfi/13)

(provide response-for)

(define (response-for msg)
  (let ([msg (string-trim-both msg)])
    (if (string=? msg "")
        "Fine. Be that way!"
        (let ([allcap?
               (and (string-index msg char-alphabetic?)
                    (string=? msg (string-upcase msg)))]
              [q?
               (eq? (string-ref msg (- (string-length msg) 1)) #\?)])
          (cond
           [(and allcap? q?) "Calm down, I know what I'm doing!"]
           [allcap? "Whoa, chill out!"]
           [q? "Sure."]
           [else "Whatever."])))))