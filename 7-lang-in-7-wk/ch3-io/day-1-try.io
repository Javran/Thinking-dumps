#!/usr/bin/env io
"* say hello to everyone:" println
"nice boat!" println

# I'm lazy and have googled how to make methods ...
printObject := method(obj,
	"======== Info Begin =========" println
	"* print object info:" println
	obj println

	"* print object type:" println
	obj type println

	"* print object slots:" println
	obj slotNames println
	"======== Info End ===========" println)

"* clone object ..." println
Vehicle := Object clone
printObject(Vehicle)

"* use slots:" println
# can only use ':=' because this slot has not yet been created
Vehicle description := "Init description"
Vehicle description println

"* change slot content:" println
# both ':=' and '=' operators will do
Vehicle description = "Something to take you places"
Vehicle description println

printObject(Vehicle)

"* type of Object:" println
Object type println

"* inheritance:" println
Car := Vehicle clone
printObject(Car)

"* try accessing 'description' of Car:" println
Car description println

"* clone Car:" println
# type should be named with capitalized words
# elsewise
ferrari := Car clone
ferrari print

"* info about 'ferrari':" println
printObject(ferrari)


"* 'Ferrari' can be a type:" println
Ferrari := Car clone
printObject(Ferrari)
