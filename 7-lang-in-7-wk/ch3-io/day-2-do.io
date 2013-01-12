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

"Task #3: add up all elements in a 2D array" println

data := list(
	list(1, 10, 100),
	list(2, 20, 150),
	list(3, 30, 290),
	list(4, 40, 350))

# luckly that 'map' and 'select'(filter) is avaliable in Io
listSum2D := method(list2D,
	return list2D map(ls, ls sum) sum)

"Data: " println
data println
"Sum: " print
listSum2D( data ) println
# 1000

"Task #4: add slot 'myAverage' to List" println

mightFail := method( 
	err := try(
		call sender doMessage(call message argAt(0)))
	err catch(Exception,
		writeln(
			"An exception raised, reason: ",
			err error)))

List myAverage := method(
	if( self size == 0,
		Exception raise("The list is empty"))
	
	if( self select(e, 
		num := e asNumber;
		(num == nil) or (num isNan)) size > 0,
		Exception raise("Some element(s) are not number(s)"))
	return (self sum) / (self size)
	)

testList := method(ls,
	"==============" println
	"Data: " println
	ls println
	"Result: " println
	mightFail( ls myAverage println )
	"==============" println)

testList( list(1, 3, 5, 5, 6) )
# 4

testList( list() )
# error: list is empty

testList( list(1, "a", 2) )
# some elements are not Numbers

"Task #5: make a prototype for 2-d arrays" println

List2D := List clone

# dim(x,y) assigns an array that contains y lists
#   each of them contains x elements
List2D dim := method(x, y, z,
	target := list()
	filler := if(z == nil, 
			method(return nil),
			z)
	for(i, 1, y, 
		subTarget := list()
		for(j, 1, x,
			subTarget append( filler(i,j) ))
		target append(subTarget) )
	return target
)

List2D dim(3,2) println

#List2D dim(3,2, method(x,y, x*10 + y) ) println
# this will cause problem, commented temporarily

