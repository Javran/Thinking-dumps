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

# TODO: remove all slots, 
# so even 'writeln' will print "<writeln></writeln>"
# * 'forward' is already disabled
# * since all slots from builder are removed
#       I'm thinking of passing context to the builder
#       to get access to functions like 'println'

XMLBuilder := Object clone
# I need a slot to store current level
XMLBuilder currentLevel := 0
XMLBuilder levelWriteln := method(content,
	for (i, 1, currentLevel, indent print)
	# how to pass all arguments to another function?
	content println)

XMLBuilder indent ::= "\t"
XMLBuilder forward := method(
	levelWriteln("<" .. call message name .. ">")
	self currentLevel := self currentLevel + 1
	call message arguments foreach(arg,
		content := self doMessage(arg)
		if (content isKindOf(Sequence),
			levelWriteln(content)))
	self currentLevel := self currentLevel - 1
	levelWriteln("</" .. call message name .. ">")
	nil)

"Builder output:" println
xmlBuilder := XMLBuilder clone
#                    _0123_
xmlBuilder setIndent("    ")
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

# TODO
