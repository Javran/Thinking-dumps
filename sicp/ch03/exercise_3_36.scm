(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./constraint_system.scm")

(define a (make-connector))
(define b (make-connector))

(set-value! a 10 'user)

#|
global(G):
(set-value! a 10 'user)

G,E1: connector = a, new-value = 10, informant = 'user
((connector 'set-value!) new-value informant)
G,E1,E2: request = 'set-value!
(cond ((eq? request 'has-value?)
        (if informant #t #f))
      ((eq? request 'value) value)
      ((eq? request 'set-value!) set-my-value)
      ((eq? request 'forget) forget-my-value)
      ((eq? request 'connect) connect)
      (else (error "Unknown operation: CONNECTOR"
                   request)))
G,E1:
(set-my-value new-value informant)

; now we want to figure out the environment of `set-my-value`:
G,E3: value = #f, informant = #f, constraints = nil

G,E3,E4: newval = 10, setter = 'user
(cond ((not (has-value? me))
        ; if I don't have any value
        (set! value newval)
        (set! informant setter)
        ; inform related constraints that something has changed
        (for-each-except setter
                         inform-about-value
                         constraints))
      ((not (= value newval))
        ; if I have a value, which does not equal to
        ;   the newval
        (error "Contradiction" (list value newval)))
      (else 'ignored))
=>
; if I don't have any value
(set! value newval)
(set! informant setter)
; inform related constraints that something has changed
(for-each-except setter
                 inform-about-value
                 constraints))
G,E3(value = 10, informant = 'user), E4:
(for-each-except setter
                 inform-about-value
                 constraints)
|#

; I won't draw the environment diagram because
;   I don't know how to draw it and everytime
;   I see it, it seems to be confusing.
; And instead, I think I'd better to demonstrate it by code
;   and text like above.

(out (get-value a))

(end-script)
