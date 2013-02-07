#!/usr/bin/env scala
!#

val regFinit = """^(F|f)\w*""".r

println( "Regexp: " + regFinit )

val testWords = "banana candy fruit favorite Fantastic"
testWords.split(" ").foreach( x => {
	println( "Word: " + x )
	val res = regFinit.findFirstIn(x)
	
	if (res.isEmpty)
		println( "Result: not found" )
	else
		println( "Result: found" )
})

val reg = "the".r
val originString = "the way the scissors trim the hair and the shrubs"

val matchIndices = reg.findAllIn( originString ).matchData.map( m => m.start ).toList

println( "Matches: " )
println( originString )
println( (0 until originString.length)
	.map( x => {
		if (matchIndices.contains( x ))
			'^'
		else
			' '
	})
	.foldLeft("")( (str, ch) => str + ch))
