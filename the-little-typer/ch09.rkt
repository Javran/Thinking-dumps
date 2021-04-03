#lang pie

(claim + (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat m
      n
      (lambda (i) (add1 i)))))

(claim incr (-> Nat Nat))
(define incr
  (lambda (n)
    (iter-Nat n
      1
      (+ 1))))

(claim incr=add1
  (Pi ((n Nat))
    (= Nat (incr n) (add1 n))))
(claim incr=add1-step
  (Pi ((n-1 Nat))
    (->
     (= Nat (incr n-1) (add1 n-1))
     (= Nat (add1 (incr n-1)) (add1 (add1 n-1))))))
#;
(define incr=add1-step
  ;; this implementation works fine
  ;; but book provides a simpler version using `cong` - see below.
  (lambda (n-1 r-incr=add1)
    ;; we want this whole thing to be:
    ;; (= Nat (add1 (incr n-1)) (add1 (add1 n-1)))
    (replace
      ;; target
      ;; - from: (incr n-1)
      ;; - to: (add1 n-1)
      r-incr=add1
      ;; motive:
      ;; - we want:
      ;;   (mot <to>)
      ;;   which is:
      ;;   (mot (add1 n-1))
      ;;   to be:
      ;;   (= Nat (add1 (incr n-1)) (add1 (add1 n-1)))
      (lambda (i) (= Nat (add1 (incr n-1)) (add1 i)))
      ;; base is a (mot <from>)
      ;; which is (mot (incr n-1))
      ;; which is (= Nat (add1 (incr n-1)) (add1 (incr n-1)))
      (same (add1 (incr n-1))))))
(define incr=add1-step
  (lambda (n-1 r-incr=add1)
    (cong r-incr=add1 (+ 1))))

(define incr=add1
  (lambda (n)
    (ind-Nat
      n
      (lambda (i) (= Nat (incr i) (add1 i)))
      (same 1)
      incr=add1-step)))
(incr=add1 4)


;; Q: what is equivalent of (cong (= X from to) f) expressed by replace?
;; (cong (= X from to) f) is of type (= Y (f from) (f to))
;; where f is of type (-> X Y).
;; so we want an expression:
;; (replace
;;  (= X from to)
;;  <mot>
;;  <base>)
;; to have the type: (= Y (f from) (f to))
;; so (<mot> <to>), which is (<mot> ?to)
;; should be: (= Y (f from) (f ?to)), therefore:
;; (replace
;;  (= X from to)
;;  (lambda (t) (= Y (f from) (f t)))
;;  <base>)
;; now what is (<mot> <from>)?
;; (= Y (f from) (f from)) ... so <base> is just (same (f from))?
;; (replace
;;  (= X from to)
;;  (lambda (t) (= Y (f from) (f t)))
;;  (same (f from)))
;; Note: I'm not 100% sure, but it looks about right.

(claim double (-> Nat Nat))
(define double
  (lambda (n)
    (iter-Nat n
      0
      (lambda (r) (add1 (add1 r))))))
(double 5)

(claim twice (-> Nat Nat))
(define twice
  (lambda (n) (+ n n)))
(twice 3)

(claim twice=double
  (Pi ((n Nat))
    (= Nat (twice n) (double n))))

(claim add1+=+add1
  (Pi ((n Nat)
       (m Nat))
    (= Nat
       (add1 (+ n m))
       (+ n (add1 m)))))
(claim add1+=+add1-step
  ;; note that here we are putting `m` in front so that
  ;; it is easier for currying.
  (Pi ((m Nat)
       (n-1 Nat))
    (->
     (= Nat
       ;; from
       (add1 (+ n-1 m))
       ;; to
       (+ n-1 (add1 m)))
      (= Nat (add1 (add1 (+ n-1 m))) (add1 (+ n-1 (add1 m)))))))
#;
(define add1+=+add1-step
  ;; works fine, but `conj` is simpler, see below.
  (lambda (m n-1 r-add1+=+add1)
    (replace
      r-add1+=+add1
      (lambda (k)
        (= Nat (add1 (add1 (+ n-1 m))) (add1 k)))
      (same (add1 (add1 (+ n-1 m)))))))

(define add1+=+add1-step
  (lambda (m n-1 r-add1+=+add1)
    (cong r-add1+=+add1 (+ 1))))

(define add1+=+add1
  (lambda (n m)
    (ind-Nat n
      ;; motive
      (lambda (i)
        (= Nat (add1 (+ i m)) (+ i (add1 m))))
      ;; base
      (same (add1 m))
      ;; step
      (add1+=+add1-step m))))

(claim twice=double-step
  (Pi ((n-1 Nat))
    (->
     (= Nat
       ;; from
       (twice n-1)
       ;; to
       (double n-1))
      ;; want:
      (= Nat
        (twice (add1 n-1))
        (double (add1 n-1))))))
#;
(define twice=double-step
  (lambda (n-1 r-twice-double)
    (replace
      r-twice-double
      ;; motive
      (lambda (k)
        (= Nat (twice (add1 n-1)) (add1 (add1 k))))
      ;; base
      (cong (symm (add1+=+add1 n-1 n-1)) (+ 1)))))

(define twice=double-step
  (lambda (n-1 r-twice-double)
    (replace
      (the
        (= Nat
          ;; "rewrite" from
          (add1 (+ n-1 n-1))
          ;; "rewrite" to
          (+ n-1 (add1 n-1)))
        (add1+=+add1 n-1 n-1))
      (lambda (k)
        ;; k is the part that "doesn't fit"
        (= Nat (add1 k) (double (add1 n-1))))
      (the
        (= Nat
          (add1
            ;; this part "doesn't fit".
            (add1 (+ n-1 n-1)))
          (double (add1 n-1)))
        (cong r-twice-double (+ 2))))))

(define twice=double
  (lambda (n)
    (ind-Nat n
      (lambda (i)
        (= Nat (twice i) (double i)))
      (same 0)
      twice=double-step)))

(claim double-Vec
  (Pi ((X U)
       (l Nat))
    (-> (Vec X l)
      ;; prefer double over twice,
      ;; as double has (add1 (add1 _)) on top
      ;; which is easier for induction to work.
      (Vec X (double l)))))
(define double-Vec
  (lambda (X l)
    (ind-Nat
      l
      ;; motive
      (lambda (k)
        (->
          (Vec X k)
          (Vec X (double k))))
      ;; base
      (lambda (_) vecnil)
      ;; step
      (lambda (l-1 r inp)
        (vec:: (head inp)
          (vec:: (head inp)
            (r (tail inp))))))))

(claim example
  (Vec Atom 7))
(define example
  (vec:: 'e
    (vec:: 'x
      (vec:: 'a
        (vec:: 'm
          (vec:: 'p
            (vec:: 'l
              (vec:: 'e
                vecnil))))))))
(double-Vec Atom 7 example)

(claim twice-Vec
  (Pi ((X U)
       (l Nat))
    (-> (Vec X l)
        (Vec X (twice l)))))
(define twice-Vec
  (lambda (X l)
    (replace
      ;; we are rewriting `double` into `twice`,
      ;; rather than the other way around.
      (symm (twice=double l))
      ;; motive
      (lambda (k) (-> (Vec X l) (Vec X k)))
      ;; base
      (double-Vec X l))))
(twice-Vec Atom 7 example)