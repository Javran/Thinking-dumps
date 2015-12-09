! enter following commands in Listener
! (TODO) for now I haven't figured out how scripting works

! 1. two digits

42 [ 10 /i ] [ 10 mod ] bi

! 2. arbitrary digits

USE: math.parser
12345 number>string [ 1string string>number ] each