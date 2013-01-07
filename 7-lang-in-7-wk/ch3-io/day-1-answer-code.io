#!/usr/bin/env io

"Task #1: test whether Io is strong typing" println

try (
	(1+1) println
) catch (Exception, e,
	"Exception raised." println
	e println)

try (
	(1+"one") println
) catch (Exception, 
	"Exception raised." println
	)
# Exception raised.


"Task #2: test bool values" println
testBool := method( v,
	"=============" println
	"object to test:" println
	v println
	if(v, 
		"returned true" println, 
		"returned false" println)
	"=============" println
)

testBool(0)
# true
testBool("")
# true
testBool(nil)
# false

"Task #3: list all slots that a prototype supports" println
TestObject := Object clone
TestObject slotA := "dummy"
TestObject slotB := "dummy"

TestObject slotNames println
# list(type, slotA, slotB) 

"Task #4: operators: ::=, :=, =" println
OperatorTest := Object clone
OperatorTest slotA ::= 1
OperatorTest slotNames println

"slotA=" print
OperatorTest slotA println

"use setter to change slotA's value into 5:" println
OperatorTest setSlotA(5)

"slotA=" print
OperatorTest slotA println

OperatorTest slotB := 2
"no setter for slotB:" println
OperatorTest slotNames println

"slotB=" print
OperatorTest slotB println

OperatorTest slotB = 4

"slotB=" print
OperatorTest slotB println
