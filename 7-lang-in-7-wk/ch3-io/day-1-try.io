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

m := method("So, you've come for an argument." println)
printObject(m)

"* type of method():" println
method() type println

"* insert method into Object..." println
Car drive := method("Vroom" println)
"* call method..." println
ferrari drive
"* slots of 'Car':" println
Car slotNames println
"* slots of 'ferrari':" println
ferrari slotNames println

# so, if 'ferrari' cannot find a slot, 
# its will ask its ancestors for help

"* get slot 'description':" println
ferrari getSlot("description") println
ferrari getSlot("drive") println
# notice here slots are assigned to 'Car' 
# rather than 'ferrari' itself

"* prototype of ferrari:" println
ferrari proto println

"* prototype of Car:" println
Car proto println

"* Lobby:" println
Lobby println

"* make a list:" println
toDos := list("find my car", "find Contiuum Transfunctioner")
toDos println

"* list size:" print
toDos size println

"* append item..." println
toDos append("find a present")

"* print list item" println
toDos println

"==== ==== list operations ==== ====" println
printList := method(l, 
	"* print list item" println
	l println)

l := list(1, 2, 3, 4)
printList(l)

"* average: " print
l average println

"* sum: " print
l sum println

"* at(1): " print
l at(1) println

"* append(4)..." println
l append(4)
printList(l)

"* pop: " print
l pop println
printList(l)

"* prepend(0)..." println
l prepend(0)
printList(l)

"* empty? " print
l isEmpty println

"==== ==== map operations ==== ====" println
elvis := Map clone
elvis proto println

"home -> Graceland" println
elvis atPut("home", "Graceland")

"style -> rock and roll" println
elvis atPut("style", "rock and roll")

"home? " print
elvis at("home") println

"as object: " println
elvis asObject println

"as list: " print
elvis asList println

"keys: " print
elvis keys println

"values: " print
elvis values println

"size: " print
elvis size println

"===========" println

(4 < 5) println
# true

(4 <= 3) println
# false

(true and false) println
# false

(true and true) println
# true

(true or false) println
# true

(4<5 and 6<7) println
# true

(true and nil) println
# false

(true and 0) println
# notice: true !

"* prototype of 'true':" println
true proto println

"* singletons: true, false, nil" println
true clone println
false clone println
nil clone println

"* make singleton:" println
Highlander := Object clone
Highlander println

Highlander clone := Highlander

"* test singleton:" println
Highlander clone println

fred := Highlander clone
mike := Highlander clone
(fred == mike) println
# true

one := Object clone
two := Object clone
(one == two) println
# false

"* test disabling clone" println
Object2 := Object clone

Object2 clone := "hosed"
ObjectA := Object2 clone
ObjectB := Object2 clone

ObjectA println
ObjectB println

"* where is the variable inside a method assigned to?" println

test := method(
	testSlot := nil
	"Inside the method:" println
	slotNames select(item, item == "testSlot") println
	"This is a test method" println)
test
"Outside the method:" println
slotNames select(item, item == "testSlot") println
