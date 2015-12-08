! enter following commands in Listener
! (TODO) for now I haven't figured out how scripting works

! 1. reduce

{ 1 4 17 9 11 } 0 [ + ] reduce .

! 2. 1 to 100

USE: math.ranges
100 [1,b] 0 [ + ] reduce .

! 3. map

10 [1,b] [ sq ] map .
