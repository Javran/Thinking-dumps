#!/usr/bin/env io

"Task #1: calculate Fibonacci number using recursion and iteration" println

"Use recursion:" println
fibRecursion := method(n,
	if( n == 1 or n == 2, return 1)
	return fibRecursion(n-1) + fibRecursion(n-2))

for(i, 1, 10, writeln(i, ":\t", fibRecursion(i)))

"Use iteration:" println
fibIteration := method(n,
	if( n == 1 or n == 2, return 1)
	a := 1
	b := 1
	for(i, 3, n,
		c := a + b
		a := b
		b := c)
	return b)

for(i, 1, 10, writeln(i, ":\t", fibIteration(i)))

"Task #2: let the '/' operator return 0 when denominator is 0" println

testDivExpr := method(expr,
	writeln("Calculate: ",expr)
	writeln("Value is: ", doString(expr)))

testDiv := method(
	testDivExpr("2/3") 		# as usual
	testDivExpr("10.123/0") 	# returns '0' in 'after'
	testDivExpr("100/10") 		# as usual
	testDivExpr("100/0.000") 	# returns '0' in 'after'
)

"================ before" println
testDiv

# try to get the original function and save it to other places
tmpDiv := Number getSlot("/")
Number / := method(y,
	if (y == 0,
		return 0,
		return self tmpDiv(y)))

"================ after" println
testDiv
