! about how to run codes here, see first bullet of `Scripting` part in `day-2/NOTES.md`
USING: math math.functions prettyprint kernel ascii sequences io ;
IN: day-1.do-easy

! 1. use only * and + to calculate 3^2 + 4^2
: ex1 ( -- ) 3 3 * 4 4 * + . ;

! 2. square root of ...

: ex2 ( -- ) 3 sq 4 sq + sqrt . ;

! 3. the question is ambiguous, let's just say "1 2" means push 1 and then 2 on the stack
: ex3 ( x y -- x x y ) over swap ;

! 4. ...

: ex4 ( x -- ) "Hello, " swap append >upper . ;
! yes, one stack shuffling (swap)

ex1
ex2
1 2 ex3 . . .
"Javran" ex4
