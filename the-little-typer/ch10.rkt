#lang pie

(claim sample
  (List Atom))

(define sample
  (:: 's
    (:: 'a
      (:: 'm
        (:: 'p
          (:: 'l
            (:: 'e
              nil)))))))
sample

(claim length
  (Pi ((E U))
    (-> (List E) Nat)))

(define length
  (lambda (E es)
    (rec-List es
      0
      (lambda (_ _ l)
        (add1 l)))))

(length Atom sample)

(claim list->vec
  (Pi ((E U)
       (es (List E)))
    (Vec E (length E es))))
(claim mot-list->vec
  (Pi ((E U))
    (-> (List E) U)))
(define mot-list->vec
  (lambda (E es)
    (Vec E (length E es))))
(define list->vec
  (lambda (E es)
    (ind-List es
      ;; motive
      (lambda (k)
        (Vec E (length E k)))
      ;; base
      vecnil
      ;; step
      (lambda (hd _ r)
        (vec:: hd r)))))

(list->vec Atom sample)