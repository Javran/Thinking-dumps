#!/usr/bin/env scala
!#

// call this script with arguments
//     (e.g. `./day-1-try-for-loop.scala foo bar`)
// to see what this for-loop will print
def javaStyleForLoop {
	println("for loop using Java-style iteration")
	for (i <- 0 until args.length) {
		println("arg-%d is: %s" format (i,args(i)))
	}
}

def rubyStyleForLoop {
	println("for loop using Ruby-style iteration")
	args.foreach { arg =>
		println("arg: " + arg)
	}
}

javaStyleForLoop
rubyStyleForLoop
