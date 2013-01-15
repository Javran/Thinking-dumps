#!/usr/bin/env io

# what is 'yield': 
# http://iolanguage.org/scm/io/docs/reference/index.html#/Core/Core/Coroutine/yield
vizzini := Object clone
vizzini talk := method(
	"V: Fezzik, are there rocks ahead?" println
	yield
	yield
	"V: No more rhymes now, I mean it." println
	yield
	yield
	wait(1))

fezzik := Object clone
fezzik rhyme := method(
	yield
	yield
	"F: If there are, we'll all be dead." println
	yield
	yield
	"F: Anybody want a peanut?" println
	wait(1))

# add another man here
thirdMan := Object clone
thirdMan say := method(
	yield
	">>> This is the third man" println
	yield
	yield
	">>> This is the third man" println)

# run these code concurrently
# will return nil immediately
vizzini @@talk; fezzik @@rhyme; thirdMan @@say

# this would wait until coroutine tasks are done.
# please refer to:
# http://iolanguage.org/scm/io/docs/reference/index.html#/Core/Core/Coroutine/pause
Coroutine currentCoroutine pause 
