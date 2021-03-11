#lang racket

(require racket/date)

(provide meetup-day)

(define (make-date year month day)
  ;; stolen from test suite
  (seconds->date (find-seconds 0 0 0 day month year #f) #f))

(define weekdays
  ;; must be ordered this way so week-day numeric indices are aligned correctly.
  '(Sunday
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday))

(define/contract (weekday->integer w)
  ;; while technically the input is to blame if we produce a #f
  ;; the very job of this function is to check for memberships,
  ;; if it's gonna fail early, I can't find a better place than here.
  (symbol? . -> . integer?)
  (index-of weekdays w))

(define (full-month year month)
  (filter-map
   (lambda (day)
     ;; definitely not pleasant, but since there is
     ;; already an internal verification logic,
     ;; I see no reason not to use it
     ;; with the alternative being implementing our own.
     (with-handlers ([exn:fail? (lambda (_) #f)])
       (make-date year month day)))
   (stream->list (in-range 1 32))))

(define which-selectors
  (let ([teenth (compose1
                 first
                 (curry filter
                        (compose1
                         (lambda (x) (<= 13 x 19))
                         date-day)))])
    ;; a happy coincidence that basic functions are named the same way.
    `((first . ,first)
      (second . ,second)
      (third . ,third)
      (fourth . ,fourth)
      (fifth . ,fifth)
      (last . ,last)
      (teenth . ,teenth))))

(define (meetup-day year month weekday which)
  (let* ([days (full-month year month)]
         [days-1
          ;; eligible days without considering `which`
          (filter (compose1
                   (curry equal? (weekday->integer weekday))
                   date-week-day)
                  days)])
    ((cdr (assq which which-selectors)) days-1)))