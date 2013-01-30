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

println( ">>> List" )

println( List(1,2,3) )
println( List("one","two","three") )
println( List("one","two",3) )
println( List("a",'b',3,4.0)(1) )
// b
println( Nil )
// Nil is an empty list

println( ">>> Set" )

val animals = Set("lions", "tigers", "bears")
println( animals )
println( animals + "armadillos" )
// set is immutable as well
println( animals - "tigers" )
// the line above won't print "armadillos"
println( animals + "armadillos" - "tigers" )
// but the line above will

// this is not the right way
// println( animals + Set( "armadillos", "raccoons" ) )

println( animals ++ Set("armadillos", "raccoons" ) )
// set union

println( animals -- Set("lions", "bears") )
// set diff

println( animals & Set("armadillos", "raccoons", "lions", "tigers") )
// set intersect

// unlike list, set is not related to the ordering of its elements
println( Set(1,2,3) == Set(3,2,1) )
// true
println( List(1,2,3) == List(3,2,1) )
// false

println( ">>> Map" )

val ordinals = Map(
	0 -> "zero",
	1 -> "one",
	2 -> "two")

println( ordinals(2) )
// two

import scala.collection.mutable.HashMap

val map = new HashMap[Int,String]

map += 4 -> "four"
map += 8 -> "eight"

println( map )
// Hashmap is mutable
// so the output will be 
//     4 -> "four", 8 -> "eight"
// note that 'val' suggest we cannot change what map is,
//     but doesn't suggest we cannot change the internal status of 'map'

// the line below will cause compilation error, because of type mismatching
// map += "zero" -> 0
