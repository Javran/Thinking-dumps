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

println( ">>> foreach" )

val list = List("frodo", "samwise", "pippin")
list.foreach(hobbit => println(hobbit))
// print elements

val hobbits = Map(
	"frodo" 	-> "hobbit0",
	"samwise" 	-> "hobbit1",
	"pippin" 	-> "hobbit2")

hobbits.foreach(hobbit_pair => println( hobbit_pair ))
// print key-value pairs (tuples of course)

hobbits.foreach(hobbit_pair => println( hobbit_pair._1))
// print keys

hobbits.foreach(hobbit_pair => println( hobbit_pair._2))
// print values

println( "The list is: " + list )
println( list.isEmpty )
// false
println( Nil.isEmpty )
// true
println( list.length )
// 3
println( list.size )
// 3

val numList = (1 to 10).toList
println( "The numList is: " + numList )

println( "head: " + numList.head )
// 1
println( "tail: " + numList.tail )
// [2..10]
println( "last: " + numList.last )
// 10
println( "init: " + numList.init )
// [1..9]
println( "reverse: " + numList.reverse )
// [10,9..1]
println( "drop(1): " + numList.drop(1) )
// [2..10]
println( "drop(10): " + numList.drop(10) )
// []

println( ">>> map! filter! fold! <<<" )
val words = List( "peg", "al", "bud", "kelly" )
println( "Word list is: " + words )

println( "How many words contain more than 2 characters? "
	+ words.count( word => word.size > 2) )

println( "What are these words? "
	+ words.filter( word => word.size > 2) )

println( "The string length of each element? "
	+ words.map( word => word.length ) )

println( "Are lengths of all words more than 1? "
	+ words.forall( word => word.size > 1) )

println( "Do we have some element whose length is more than 4? "
	+ words.exists( word => word.size > 4 ) )

println( "What are these words? "
	+ words.filter( word => word.size > 4 ) )

println( "Do we have some element whose length is more than 5? "
	+ words.exists( word => word.size > 5 ) )

println( "Sort elements by first char's alphabetic order:" )
println( words.sortWith( (a,b) => a.charAt(0).toLower < b.charAt(0).toLower) )

println( "Sort elements by word length:" )
println( words.sortWith( (a,b) => a.size < b.size ) )

println( "Sort elements by word length and first char:" )
println( words.sortWith( 
	(a,b) => {
		if (a.size != b.size)
			a.size < b.size
		else
			a.charAt(0).toLower < b.charAt(0).toLower
	} ) )

println( ">>> foldL! <<<" )
println( "Calculate sum of [1..100]:" )
val listToSumUp = (1 to 100).toList

println( "Approach #1: operator '/:' " )
println( (0 /: listToSumUp) {(sum,i) => sum+i} )

println( "Approach #2: foldLeft: " )
println( listToSumUp.foldLeft(0)( (acc,i) => acc+i ) )

println( "Approach #3: foldLeft(head as seed): " )
println( listToSumUp.tail.foldLeft(listToSumUp.head)( (acc,i) => acc+i ) )

println( "Approach #4: foldRight: " )
println( listToSumUp.foldRight(0)( (i,acc) => i+acc) )
