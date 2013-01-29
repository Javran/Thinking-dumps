#!/usr/bin/env scala
!#

// define a function
def double(x:Int):Int = x * 2

println( double(123)+210 )
// 456

def doubleBlk(x:Int):Int = {
	x * 2
}

println( double(321)+147 )
// 789

// var means variable
var mutable = "I am mutable"
mutable = "Touch me, change me.."

// val means value
val immutable = "I am not mutable"
//immutable = "Can't touch this"
// the line above will cause error
//     for which is a value and cannot be changed
