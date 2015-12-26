USING: math prettyprint sequences ;
IN: day-1.do-medium

! 1. reduce
: ex1 ( -- )
{ 1 4 17 9 11 } 0 [ + ] reduce . ;

! 2. 1 to 100
USE: math.ranges 

: ex2 ( -- )
100 [1,b] 0 [ + ] reduce . ;

! 3. map
: ex3 ( -- )
10 [1,b] [ sq ] map . ;

ex1
ex2
ex3
