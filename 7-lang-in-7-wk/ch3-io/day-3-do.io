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
		) foreach(l, l println)
	"==== END ====" println)

# I don't know how to break the message forward mechanism
#     which is not controlled by 'forward' (which we've created 
#     our own and overwritten the original one.
# so there's a bug that existing functions are still work
#     e.g. 'println' will not print "<println></println>"

# It will be better if we could return a list of things to print
# thus we could do indentation freely
XMLBuilder := Object clone
XMLBuilder indent ::= "\t"
XMLBuilder forward := method(
	head := "<#{call message name}>" interpolate
	contents := call message arguments map(arg, self doMessage(arg))
	contentList := contents flatten map(l, indent .. l)
	tail := "</#{call message name}>" interpolate

	contentList atInsert(0, head)
	contentList append(tail)
	contentList)

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
OperatorTable addAssignOperator(":", "atPutNumber")

curlyBrackets := method(
	r := Map clone
	call message arguments foreach(arg,
		r doMessage(arg))
	r)
Map atPutNumber := method(
	self atPut(
		call evalArgAt(0) asMutable \
			removePrefix("\"") \
			removeSuffix("\""),
		call evalArgAt(1)	
	)
)

XMLBuilderPlus := XMLBuilder clone
XMLBuilderPlus forward := method(

	# first evaluate all arguments
	# the elements will be either Map or List
	contents := call message arguments map(arg, self doMessage(arg))

	# now we collect all Maps
	attributes := contents select(e, e isKindOf(Map))
	attributeContent := attributes map(m, 
		# each Map will be expanded to list of Key-Value
		m asList map(kv, 
			# convert pairs to string
			# flatten the list and join them together
			"#{kv at(0)}=\"#{kv at(1)}\"" interpolate)) \
			flatten join(" ")

	elements := contents select(e, e isKindOf(List))
	# flatten the element list, and do indentation
	elementContentList := elements flatten map(e, indent .. e)

	attr := if (attributeContent size > 0,
			" " .. attributeContent,
			"")

	head := "<#{call message name}#{attr}>" interpolate
	tail := "</#{call message name}>" interpolate
	elementContentList atInsert(0,head)
	elementContentList append(tail)
	elementContentList)

builder := XMLBuilderPlus clone
#                 _0123_
builder setIndent("    ");
xmlFile := File with("day-3-do-xml.txt") openForReading
result := doString("builder #{xmlFile contents}" interpolate)
result foreach(l, l println)
xmlFile close
