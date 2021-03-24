(ns armstrong-numbers)

;; Regarding why not Math/pow - it's designed for double not int,
;; and IEEE754 double doesn't have sufficient precision to produce correct results:
;;
;; user=> (apply * (repeat 25 9N))
;; 717897987691852588770249N
;; user=> (bigint (Math/pow 9 25))
;; 717897987691852600000000N

(defn expt
  "compute n^m, where n >= 0, m > 0."
  [n init-m]
  ;; https://en.wikipedia.org/wiki/Exponentiation_by_squaring
  (if (zero? n)
    0
    (loop [result 1N
           m init-m
           acc (bigint n)]
      (if (= m 0)
        result
        (recur
         (if (even? m)
           result
           (* result acc))
         (quot m 2)
         (* acc acc))))))

;; This is the straightforward version of `expt` above.
(defn expt-simple
  "compute n^m, where n >= 0, m > 0."
  [n m]
  (apply * (repeat m (bigint n))))

(defn armstrong? [num]
  (let [s (str num)
        m (count s)
        pows (map
              (comp
               #(expt % m)
               #(Character/digit % 10))
              s)]
    (=
     num
     (apply + pows))))
