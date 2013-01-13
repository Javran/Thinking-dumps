#!/usr/bin/env io

# what's the difference between AssignOperator and Operator?
# I think: 
# AssignOperators are unary operators, 
#     assume that ":::=" is an AssignOperator and bound to "assignOp"
#     `a :::= b` will be parsed as `assignOp("a",b)`
#
# Operators are unary operators,
#     assume that "!" is an Operator and bound to "op"
#     `a ! b` will be parsed as `a op(b)`
# is that right?
OperatorTable addAssignOperator(":", "atPutNumber")

# where can I get the document 
#     saying "{","}" will be regarded as "curlyBrackets" ?
# I've found a document:
#     http://iota.flowsnake.org/syntax-extensions.html
# seems to be a very advanced tech...
curlyBrackets := method(
	r := Map clone
	# 'call message arguments' is a list
	#     which can be examined here:
	#         `call message arguments type println`
	#     will print 'List'
	call message arguments foreach(arg,
		r doMessage(arg))
	r)

Map atPutNumber := method(
	self atPut(
		call evalArgAt(0) asMutable \
			removePrefix("\"") \
			removeSuffix("\""),
		call evalArgAt(1)))

s := File with("day-3-try-phonebook.txt") openForReading contents

phoneNumbers := doString(s)

"Print keys:" println
phoneNumbers keys println

"Print values:" println
phoneNumbers values println

"Print key-value pairs:" println
phoneNumbers foreach(k, v, writeln(k, " : ", v))
