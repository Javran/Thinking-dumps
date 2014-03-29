(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; dependencies:
;; who:         related to:
;; Baker        -
;; Cooper       Miller, Fletcher
;; Fletcher     Smith, Cooper
;; Miller       Cooper
;; Smith        Fletcher

;; planned order of enumeration:
;; - Fletcher
;; - Cooper
;; - Miller
;; - Smith
;; - Baker

(define solutions '())

(define (not-adjecent? a b)
  (> (abs (- a b)) 1))

(define (distinct-list? xs)
  (or (null? xs)
      (and (not (memq (car xs) (cdr xs)))
           (distinct-list? (cdr xs)))))

(define (try-fletcher fletcher)
  (if (> fletcher 5)
      'done
      (begin
        ;; 4. Fletcher does not live on either
        ;; the top or the botton floor
        (if (and (> fletcher 1)
                 (< fletcher 5))
            (try-cooper fletcher 1)
            ;; else
            'skip)
        (try-fletcher (add1 fletcher)))))

(define (try-cooper fletcher cooper)
  (if (> cooper 5)
      'done
      (begin
        (if (and
             ;; 3. Cooper does not live on the bottom floor
             (> cooper 1)
             ;; 7. Fletcher does not live on a floor
             ;; adjacent to Cooper's
             (not-adjecent? fletcher cooper))
            (try-miller fletcher cooper 1)
            ;; else
            'skip)
        (try-cooper fletcher (add1 cooper)))))

(define (try-miller fletcher cooper miller)
  (if (> miller 5)
      'done
      (begin
        ;; 5. Miller lives on a higher floor than does Cooper
        (if (> miller cooper)
            (try-smith fletcher cooper miller 1)
            ;; else
            'skip)
        (try-miller fletcher cooper (add1 miller)))))

(define (try-smith fletcher cooper miller smith)
  (if (> smith 5)
      'done
      (begin
        ;; 6. Smith does not live on a floor adjacent
        ;; to Fletcher's
        (if (not-adjecent? smith fletcher)
            (try-baker fletcher cooper miller smith 1)
            ;; else
            'skip)
        (try-smith fletcher cooper miller (add1 smith)))))

(define (try-baker fletcher cooper miller smith baker)
  (if (> baker 5)
      'done
      (begin
        ;; 2. Baker does not live on the top floor
        (if (< baker 5)
            (try-distinct fletcher cooper miller smith baker)
            ;; else
            'skip)
        (try-baker fletcher cooper miller smith (add1 baker)))))

(define (try-distinct fletcher cooper miller smith baker)
  ;; 1. They live on different floors
  (if (distinct-list? (list fletcher cooper miller smith baker))
      (let ((solution
             (list
              `(fletcher ,fletcher)
              `(cooper   ,cooper)
              `(miller   ,miller)
              `(smith    ,smith)
              `(baker    ,baker)
              )))
        (set! solutions (cons solution solutions)))
      ;; else
      'skip))

(try-fletcher 1)

(out solutions)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
