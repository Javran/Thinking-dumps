#!/usr/bin/env scala
!#

// if you encounter some problem like:
//     "Unable to establish connection to compilation daemon"
// please make sure to set your ip-hostname map properly in /etc/hosts
// related link:
//     http://stackoverflow.com/questions/7714696/scala-hello-world-script-does-not-work
println("nice boat!")
// nice boat!

println(1+1)
// 2

println((1).+(1))
// 2

println(5 + 4 * 3)
// 17

println(5.+(4.*(3)))
// 17.0

println((5).+((4).*(3)))
// 17

println((5.).+(4.*(3)))
// 17.0

println( "abc".size )
// 3

println( "abc" + 4 )
// abc4

println( 4 + "abc" )
// 4abc

println( 4 + "1.0" )
// 41.0 <- note that this is a string

println( "abc" * 4 )
// it will produce 4 "abc"s

//println( 4 * "abc" )
// scala is strong-typed and 
//     the line above will cause an error
//     which I guess is a compilation error
//     because try-catch statement 
//     cannot catch this kind of error

// FYI:
// the block comment below shows one way to do error handling
/*
try {
	throw new Exception
} catch {
	case ex: Exception => {
		println("An exception occurred.")
	}
}
*/

println( ">>> test Booleans" )
println( 5 < 6 )
// true

println( 5 <= 6 )
// true

println( 5 <= 2 )
// false

println( 5 != 2 )
// true

val a = 1
val b = 2

if (b < a) {
	println( "True." )
} else {
	println( "False.")
}

println( Nil )
// Nil is a list?

// these lines below will never work
// if(0) {println( "true" )}
// if(Nil) {println( "true" )}

def whileLoop {
	var i = 1
	while (i <= 3) {
		println("while loop: %d" format i)
		i += 1
	}
}

whileLoop

val range = 0 until 10
// start,end,step = 0,10,1

def testRange(range:Range) {
	println("Range: " + range)
	println("Range start: " + range.start)
	println("Range end  : " + range.end)
	println("Range step : " + range.step)
}

testRange( range )
testRange( (0 to 10) by 5 )
// note: 'to' includes the end
// (0,5,10)
testRange( (0 to 10) by 6 )
// (0,6)
testRange( (0 until 10) by 5)
// note: 'until' excludes the end
testRange( (10 until 0) by -1)
// (10 .. 1)
testRange( (10 to 0) by -1)
// (10 .. 0)
testRange( (10 until 0) )
// ()
testRange( (0 to 10) )
// (1.. 10)
println( 'a' to 'e' )

val person = ("Elvis", "Presley")
println( person._1 )
println( person._2 )

val (x,y) = (1,2)
println( x )
println( y )
// 1 and 2

class Person(firstName : String, lastName : String)
val gump = new Person("Forrest", "Gump")
println( gump )
