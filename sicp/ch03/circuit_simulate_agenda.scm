(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; DON'T USE THESE CONFUSING STUFFS `(list 0)` WHEN
;   WHAT YOUR ARE DOING IS TO CREATE A PAIR
;   RATHER THAN A LIST

; make an agenda with time=0, no time segments
(define (make-agenda) (cons 0 nil))
(define current-time car)
(define segments cdr)

(define set-current-time! set-car!)
(define set-segments! set-cdr!)

(define first-segment (compose car segments))
(define rest-segments (compose cdr segments))

(define empty-agenda? (compose null? segments))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda
                         (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define (add-to-agenda! time action agenda)
  ; insert-inplace? tests if the given action should be
  ;   insert in front of the `segments`
  (define (insert-inplace? segments)
        ; if `segments` is empty
    (or (null? segments)
        ; or if the first segment from `segments`
        ;   has a newer (greater) time than
        ;   the action we want to add into
        (< time (segment-time (car segments)))))
  ; make a time segment using `time` and `action`
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      ; the head is exactly the time we want
      ;   put the action into that queue
      (insert-queue! (segment-queue (car segments))
                     action)
      ; else
      (let ((rest (cdr segments)))
        (if (insert-inplace? rest)
          ; if we should insert in place,
          ;   make the segment and put it here
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          ; else this is not the right place,
          ;   keep going
          (add-to-segments! rest)))))
  ; so what's the benefit of confusing others with
  ;   shadowing the definition of `segments` here?
  (let ((segments (segments agenda)))
    (if (insert-inplace? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))
