USING: prettyprint ;
IN: day-1.do-hard

! 1. two digits
USING: math kernel ;
: ex1 ( x -- a b )
    [ 10 /i ] [ 10 mod ] bi 
;

! 2. arbitrary digits

USING: math.parser strings sequences ;
! I have to print it out rather than keeping it on the stack
! because I don't know the correct stack effect signature
: ex2 ( x -- )
    number>string [ 1string string>number . ] each
;

45 ex1 . .
12345 ex2
