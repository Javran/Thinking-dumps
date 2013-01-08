#!/usr/bin/env io

"Task #1: run io code from file" println
"That's it!" println

"Task #2: run code in a slot given its name" println

TestObject := Object clone

TestObject slotA := method(
	"This is slotA!" println
)

TestObject slotB := method(
	"slotB fired!" println
)

# use 'doString' to eval a str!
runSlot := method( slotName,
	("Try to fire: " .. slotName ) println
	doString("TestObject ".. slotName)
)

runSlot("slotA")
runSlot("slotB")
runSlot("type println")
runSlot("slotNames println")
