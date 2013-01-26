#!/usr/bin/env scala "$0" "$@"
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
// the line above will cause an error
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


