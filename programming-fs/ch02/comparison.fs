// note this file only works in interactive mode

10 > 20;; // false
10 = 20;; // false
10 < 20;; // true

10 <> 20;; // true

"abc" = "abcd";; // false

// 'compare' acts just like spaceship operator

compare 1 10;; // -1
compare "a" "a";; // 0
compare 100.0 10.0;; // 1

#quit;;
