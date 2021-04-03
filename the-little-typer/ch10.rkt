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
(define list->vec
  (lambda (E es)
    (ind-List es
      ;; motive
      (lambda (k)
        ;; I made a mistake early on that
        ;; I used `es` instead of `k` here -
        ;; I wish that the language can tell me
        ;; when a variable is unused.
        (Vec E (length E k)))
      ;; base
      vecnil
      ;; step
      (lambda (hd _ r)
        (vec:: hd r)))))

(list->vec Atom sample)