USING: kernel sequences prettyprint ;
IN: day-2.do-easy

: palindrome? ( seq -- bool )
    dup reverse = ;

"abcde" palindrome? .
"abcba" palindrome? .
