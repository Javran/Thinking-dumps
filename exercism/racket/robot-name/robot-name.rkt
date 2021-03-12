#lang racket

(provide make-robot
         name
         reset!
         reset-name-cache!)


;; notes regarding this design:
;; while using a single mutable-set is possible,
;; this solution also makes sure when a name is reset, that name is returned to the "global name pool"
;; and can be used by any robot but those owned it before.

(define robot? integer?)

(define robot-count 0)

(define all-names
  ;; stores all existing names (excluding reset names)
  (mutable-set))
(define current-names
  ;; stores a mapping from robot id to its name
  (make-hash))
(define all-used-names
  ;; stores a mapping from robot id to a mutable-set of all its used names
  (make-hash))

(define (random-char a b)
  (integer->char
   (random (char->integer a) (+ (char->integer b) 1))))

(define (gen-name-naive)
  ;; generates a new random name without considering name collision
  (string
   (random-char #\A #\Z)
   (random-char #\A #\Z)
   (random-char #\0 #\9)
   (random-char #\0 #\9)
   (random-char #\0 #\9)))

(define (gen-name-for robot)
  ;; generates a new name for a robot.
  ;; note that it is assumed by this function that robot does not have a current-names mapping.
  (let ([used-names (hash-ref
                     all-used-names
                     robot
                     (lambda ()
                       (let ([x (mutable-set)])
                         (hash-set! all-used-names robot x)
                         x)))])
    (let redo ([n (gen-name-naive)])
      (if (or (set-member? all-names n)
              (set-member? used-names n))
          (redo (gen-name-naive))
          (begin
            (set-add! all-names n)
            (set-add! used-names n)
            (hash-set! current-names robot n)
            n)))))

(define (make-robot)
  (set! robot-count (+ robot-count 1))
  robot-count)

(define (name robot)
  (hash-ref
   current-names
   robot
   (lambda () (gen-name-for robot))))

(define (reset! robot)
  (let ([n (hash-ref
            current-names
            robot
            #f)])
    (and n
         (begin
           (set-remove! all-names n)
           (hash-remove! current-names robot)))))

(define (reset-name-cache!)
  ;; it is intentional that robot-count doesn't reset, this ensures the uniqueness of every robot
  ;; through one execution.
  (set-clear! all-names)
  (hash-clear! current-names)
  (hash-clear! all-used-names))
