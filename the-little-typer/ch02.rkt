#lang pie

;; (the _ _)
'spinach

;; 'spinach is not a type
;; (car 'spinach)

;; need to type explicitly.
(the (Pair Atom Atom)
  (cons 'spinach 'cauliflower))

;; eliminator
(car (the (Pair Atom Atom)
       (cons 'spinach 'cauliflower)))

;; a mixture of types
(Pair
  (-> U U)
  (Pair Atom U))