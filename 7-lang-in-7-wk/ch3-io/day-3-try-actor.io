#!/usr/bin/env io

slower := Object clone
faster := Object clone

slower start := method(
	"slower begin" println
	wait(2)
	"slower done" println)

faster start := method(
	"faster begin" println
	wait(1)
	"faster done" println)


runByOrder := method(
	"Activate 'slower' & 'faster' by order ..." println
	s_value := slower start
	f_value := faster start
	writeln("'slower' returns: ", s_value)
	writeln("'faster' returns: ", f_value)
)

runConcurrently := method(
	"Activate 'slower' & 'faster' concurrently ..." println
	s_value := slower @@start
	f_value := faster @@start
	writeln("'slower' returns: ", s_value)
	writeln("'faster' returns: ", f_value)
	"Waiting ..." println
	wait(3)
	"Done." println
)

runByOrder
runConcurrently
