## Discussion on how to use scala files

Discussions can be found at:

[A Scala shell script example (and discussion)](http://alvinalexander.com/scala/scala-shell-script-example-exec-syntax)

You can find my scala template make use of [Shebang](http://en.wikipedia.org/wiki/Shebang_%28Unix%29) to do the trick:

	#!/usr/bin/env scala
	!#

remember not to miss `!#` in the second line.

If [Man page](http://en.wikipedia.org/wiki/Man_page) is avaliable, you can also find another approach by looking up `man scala`:

	#!/bin/sh
	exec scala "$0" "$@"
	!#
	Console.println("Hello, world!")
	argv.toList foreach Console.println

The lines above only work for linux.

For windows, solution can be found at scala's man page as well.

## Difference between "closure" and "code block"

Discussion can be found at:

[Exactly what is the difference between a "closure" and a "block"?](http://stackoverflow.com/questions/1812401/exactly-what-is-the-difference-between-a-closure-and-a-block)
