! enter following commands in Listener
! (TODO) for now I haven't figured out how scripting works

! 1. use only * and + to calculate 3^2 + 4^2

3 3 * 4 4 * + .

! 2. square root of ...

USE: math.functions 
3 sq 4 sq + sqrt

! 3. the question is ambiguous, let's just say "1 2" means push 1 and then 2 on the stack
over swap

! 4. ...

"Javran" "Hello, " swap append >upper .
! yes, one stack shuffling (swap)