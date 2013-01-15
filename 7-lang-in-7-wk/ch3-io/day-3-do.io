#!/usr/bin/env io

"Task #1: Modify XML Builder to produce pretty output" println

builderTester := method(builder,
	"=== BEGIN ===" println
	builder \
		ul (
			li("Languages"),
			ul(
				li("Io"),
				li("Lua"),
				li("JavaScript")
			),
			ul(
				li("Ruby"),
				li("Perl"),
				li("Python")
			)
		)
	"==== END ====" println)

# I don't know how to break the message forward mechanism
#     which is not controlled by 'forward' (which we've created 
#     our own and overwritten the original one.
# so there's a bug that existing functions are still work
#     e.g. 'println' will not print "<println></println>"

XMLBuilder := Object clone

# I need a slot to store current level
# Maybe using State will be better
XMLState := Object clone
XMLState indent ::= "\t"
XMLState currentLevel ::= 0

XMLBuilder state ::= XMLBuilder XMLState clone

XMLBuilder levelWriteln := method(content,
	self state currentLevel \
		repeat( self state indent print )
	# how to pass all arguments to another function?
	content println)

XMLBuilder forward := method(
	levelWriteln("<#{call message name}>" interpolate)
	self state currentLevel := self state currentLevel + 1
	call message arguments foreach(arg,
		content := self doMessage(arg)
		if (content isKindOf(Sequence),
			levelWriteln(content)))
	self state currentLevel := self state currentLevel - 1
	levelWriteln("</#{call message name}>" interpolate)
	nil)

"Builder output:" println
xmlBuilder := XMLBuilder clone
#                          _0123_
xmlBuilder state setIndent("    ")
builderTester(xmlBuilder)

"Task #2: Create a list syntax that uses brackets" println
# set up an environment to constrain scope
#     that the modified 'squareBrackets' would affect.
BracketSyntaxEnv := method(
	# let's use square brackets to represent a list
	# and everything is separated by comma
	squareBrackets := method(
		call message arguments map(arg,self doMessage(arg))
	)
	# even embeded structure is supported!
	[
		["This"],
		"is",
		["a","list"], 
		"that contains objects",
		["like", 
			[1,2,true,false,nil]
		]
	] println
)

BracketSyntaxEnv

"Task #3: Add attribute support for XML Builder" println 

XMLBuilderPlusEnv := method(
)

XMLBuilderPlusEnv
