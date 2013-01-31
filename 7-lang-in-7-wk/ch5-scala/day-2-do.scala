#!/usr/bin/env scala
!#

println( "Task #1: compute total length of a list of strings" )

val testString = 
	"we need to count every word in this list"

val listToCount = testString.split(' ').toList

println( "The list is: " + listToCount )

println( "Original string is: " + testString )

println( "Target count: " + listToCount.foldLeft(0)(
	(acc, str) => acc + str.length) )

println( "Verify result: " + testString.count(ch => ch != ' ') )

println

println( "Task #2: write Censor trait" )

class ContentPrinter {
	
	def print(str:String) = {
		println( str )
	}

}

// TODO maybe this is not the right way of doing it...
//     it's really pointless to use a trait only because we want a new method
//     and this damn method do not even know what class it's been added to...
trait Censor {
	val defaultCurseWordMap = Map(
		"Pucky" -> "Shoot",
		"Beans" -> "Darn")
	
	def censoredPrint(str:String, map:Map[String,String] = defaultCurseWordMap) = {
		println( censorWithMap(str, map) )	
	}

	def censorWithMap(str:String, map: Map[String,String]) = {
		map.foldLeft(str) ( (curStr,kv) => { kv._1.r.replaceAllIn(curStr, kv._2) } )
	}

}

val cp = new ContentPrinter with Censor

val originStr = "Pucky! Beans! We need to keep these curse words out of my sight."

println( "Original content:" )
cp.print( originStr )
println( "Censored content:" )
cp.censoredPrint( originStr )

println

println( "Task #3: load the curse words and alternatives from a file" )

// stub
