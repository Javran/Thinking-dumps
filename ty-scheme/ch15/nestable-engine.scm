; please use Guile for this source file
(load "../common/guile/utils.scm")
(load "../common/guile/clock.scm")

(define make-engine
  (lambda (th)
    (lambda (ticks on-success on-failure)
      (let* ((parent-ticks
               (clock 'set *infinity*))

             ; a child can't have more ticks than its parent's
             ; remaining ticks
             (child-available-ticks
               (clock-min parent-ticks ticks))

             ; a child's ticks must be counted against the parent too
             (parent-ticks-left
               (clock-minus parent-ticks child-available-ticks))

             ; if child was promised more ticks than parent could afford,
             ; remember how much it was short-changed by
             (child-ticks-left
               (clock-minus ticks child-available-ticks))

             ; used below to store ticks left in clock
             ; if child completes in time
             (ticks-left 0)

             (engine-succeeded? #f)

             (result 
               (fluid-let ((*engine-escape* #f)
                           (*engine-entrance* #f))
                          (call/cc
                            (lambda (k)
                              (set! *engine-escape* k)
                              (let ((result
                                      (call/cc
                                        (lambda (k)
                                          (set! *engine-entrance* k)
                                          (clock 'set child-available-ticks)

                                          (let ((v (th)))
                                            (*engine-entrance* v))))))
                                (set! ticks-left
                                  (let ((n (clock 'set *infinity*)))
                                    (if (eqv? n *infinity*) 0 n)))
                                (set! engine-succeeded? #t)
                                result))))))

        ; parent can reclaim ticks that child didn't need
        (set! parent-ticks-left
          (clock-plus parent-ticks-left ticks-left))

        ; this is the true ticks that child has left --
        ; we include the ticks it was short-changed by
        (set! ticks-left
          (clock-plus child-ticks-left ticks-left))

        ; restart parent with its remaining ticks
        (clock 'set parent-ticks-left)

        ; the rest is now parent computation

        (cond
          ; child finished in time -- celebrate its success
          (engine-succeeded? (on-success result ticks-left))

          ; child failed because it ran out of promised time --
          ; call failure procedure
          ((= ticks-left 0)
           (on-failure (make-engine (lambda () (result 'resume)))))

          ; child failed because parent didn't have enough time,
          ; ie, parent failed too. 
          ; if so, when parent is resumed,
          ; its first order of duty is to resume the child
          ; with its fair amount of ticks
          (else
            ((make-engine (lambda () (result 'resume)))
             ticks-left on-success on-failure)))))))


(out (clock 'set 1))
(sleep 10)
