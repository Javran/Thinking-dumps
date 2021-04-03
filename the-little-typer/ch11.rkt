#lang pie

(claim + (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat m
      n
      (lambda (i) (add1 i)))))

(claim foo
  (Vec Atom 3))
(define foo
  (vec:: 'f
    (vec:: 'o
      (vec:: 'o
        vecnil))))
(claim bar
  (Vec Atom 3))
(define bar
  (vec:: 'b
    (vec:: 'a
      (vec:: 'r
        vecnil))))

(claim vec-append
  (Pi ((E U)
       (l Nat)
       (r Nat))
    (-> (Vec E l)
        (Vec E r)
        (Vec E (+ l r)))))
(define vec-append
  (lambda (E l-len r-len ls rs)
    (ind-Vec l-len ls
      ;; motive
      (lambda (k-len _)
        (Vec E (+ k-len r-len)))
      ;; base
      rs
      ;; step
      (lambda (_ l-hd _ r)
        (vec:: l-hd r)))))

(claim foofoobar (Vec Atom 9))
(define foofoobar
  (vec-append Atom 6 3
    (vec-append Atom 3 3 foo foo)
    bar))
foofoobar

;; `length` and `list->vec` from ch10.rkt
(claim length
  (Pi ((E U))
    (-> (List E) Nat)))

(define length
  (lambda (E es)
    (rec-List es
      0
      (lambda (_ _ l)
        (add1 l)))))

(claim list->vec
  (Pi ((E U)
       (es (List E)))
    (Vec E (length E es))))
(define list->vec
  (lambda (E es)
    (ind-List es
      (lambda (k)
        (Vec E (length E k)))
      vecnil
      (lambda (hd _ r) (vec:: hd r)))))

(claim vec->list
  (Pi ((E U)
       (l Nat))
    (-> (Vec E l)
        (List E))))
(define vec->list
  (lambda (E l)
    ;; book uses ind-Vec, but ind-Nat will also do.
    (ind-Nat l
      (lambda (k)
        (-> (Vec E k) (List E)))
      (lambda (_) nil)
      (lambda (n-1 r xs)
        (:: (head xs)
          (r (tail xs)))))))
(vec->list Atom 3 foo)

(claim list->vec->list=
  (Pi ((E U)
       (es (List E)))
    (= (List E)
       es
       (vec->list E
         (length E es)
         (list->vec E es)))))
(claim mot-list->vec->list=
  (Pi ((E U))
    (-> (List E) U)))
(define mot-list->vec->list=
  (lambda (E es)
    (= (List E) es (vec->list E (length E es) (list->vec E es)))))
(claim step-list->vec->list
  (Pi ((E U)
       (hd E)
       (tl (List E)))
    (-> (mot-list->vec->list= E tl)
        (mot-list->vec->list= E (:: hd tl)))))
(define step-list->vec->list
  (lambda (E hd tl r-eq)
    (cong r-eq
      (the
        (-> (List E) (List E))
        (lambda (k) (:: hd k))))))
(define list->vec->list=
  (lambda (E es)
    (ind-List es
      (mot-list->vec->list= E)
      (same nil)
      (step-list->vec->list E))))
(list->vec->list= Atom (:: 'a (:: 'b (:: 'z nil))))