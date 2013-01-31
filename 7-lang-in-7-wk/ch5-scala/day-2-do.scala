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

println( "Task #2: write Censor trait" )

// stub

println( "Task #3: load the curse words and alternatives from a file" )

// stub
