#!/usr/bin/env io

Object ancestors := method(
	prototype := self proto
	if (prototype != Object,
		writeln( "Slots of ", prototype type)
		writeln( "========================")
		prototype slotNames foreach(slotName, writeln(slotName))
		writeln
		prototype ancestors))

Animal := Object clone
Animal speak := method(
	"ambigious animal noise" println)

Duck := Animal clone
Duck speak := method(
	"quack" println)

Duck walk := method(
	"waddle" println)

disco := Duck clone
disco ancestors
# Let me guess what happened:
# send message 'ancestors' to 'disco'
# and 'Object' response to 'ancestors' message
# disco runs the method in slot
# disco prints names of all the slots it has
# an 'ancestors' message is again passed to its prototype

# assign myself an additional mission: 
# fire all the slots recursively excluding 'type' slot
fireAllSlotsOf := method(obj,
	prototype := obj proto
	if (prototype != Object,
		writeln("Slots of ", prototype type, ":")
		writeln("=======================")
		prototype slotNames foreach(slotName,
			writeln("The slot name is: ", slotName)
			if (slotName == "type",
				"/* This slot will not be fired. */" println,
				"/* Fire this slot...            */" println
				doString("prototype " .. slotName))
		)
		fireAllSlotsOf(prototype)))

fireAllSlotsOf(disco)
