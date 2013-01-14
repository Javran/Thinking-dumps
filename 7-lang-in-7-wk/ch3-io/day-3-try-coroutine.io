#!/usr/bin/env io

# I want to know what is 'yield' here

vizzini := Object clone
vizzini talk := method(
	"V: Fezzik, are there rocks ahead?" println
	yield
	"V: No more rhymes now, I mean it." println
	yield)

fezzik := Object clone
fezzik rhyme := method(
	yield
	"F: If there are, we'll all be dead." println
	yield
	"F: Anybody want a peanut?" println)

# what is this?
vizzini @@talk; fezzik @@rhyme

# and what is this?
Coroutine currentCoroutine pause
