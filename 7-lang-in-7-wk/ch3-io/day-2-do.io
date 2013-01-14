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
# please refer to:
#     http://stackoverflow.com/questions/14294780/how-to-pass-a-method-but-not-activate-it-in-io-language
# and my solution is here:
#     https://gist.github.com/4527872
/*
# old method, still work, but you should pass method as code string
# quick and dirty way :)
List2D dim := method(x, y, z,
	target := List2D clone
	filler := if(z == nil, 
			"method(return nil)",
			z)
	doString("filler := " .. z)
	for(i, 1, y, 
		subTarget := list()
		for(j, 1, x,
			subTarget append(filler(i,j) ))
		target append(subTarget) )
	target cols := y
	target rows := x
	return target
)
*/
List2D dim := method(x, y,
	target := List2D clone 
	default := call evalArgAt(2)
	# to improve performance, type will be compared only once.
	# /* bad practice */
	# if(default type == "Block", 
	# follow the advice that using 'isKindOf' 
	#     rather than comparing against type slot 
	if(default isKindOf(Block), 
		for(i, 1, y,
			subTarget := list()
			for(j, 1, x,
				subTarget append( default call(i,j) )
			)
			target append(subTarget)
		),
		# elsewise
		for(i, 1, y,
			subTarget := list()
			for(j, 1, x,
				subTarget append( default )
			)
			target append(subTarget)
		)
	)

	target cols := y
	target rows := x
	target)

data := List2D dim(3,2)

"Result:" println
data println

"Create a 2-d array with default value:" println

"Result:" println
List2D dim(3,2,"foo") println

"Create a 2-d array with 'filler':" println
List2D dim(2,3, block(a,b, if( (a+b) % 2 == 0, "foo", "bar"))) println
List2D dim(3,2, block(x,y, 
		"<" .. (x asString) .. 
		"," .. (y asString) .. ">" ) ) println

# this will cause problem, commented temporarily

List2D set := method(x, y, newVal,
	at(y) atPut(x, newVal))	

List2D get := method(x, y,
	at(y) at(x))

"Elements -> <row, col>" println
# set each element to tuple <row,col>

for(i, 0, 2,
	for(j, 0, 1,
		data set(i,j,list(i,j))))

"Use 'get' to print the list:" println
for(j, 0, 1,
	for(i, 0, 2,
		element := data get(i,j)
		write("<", element at(0), ",", element at(1), "> "))
	"" println)

# now overwrite println !
List2D println := method( separator,
	if (separator == nil, separator = " ")
	writeln("col:", cols, ", row:",rows)
	for(j, 0, cols-1,
		for(i, 0, rows-1,
			element := get(i,j)
			element print
			separator print)
		"" println))

"Use customed 'println':" println
data println

"Task #6: Matrix transposition:" println
List2D transpose := method(
	target := List2D dim(cols,rows)
	for(j, 0, target cols-1,
		for(i, 0, target rows-1,
			target set(i,j, get(j,i))))
	
	return target)

data transpose println

transposeVerifier := method(matrixA, matrixB,
	"Checking <rows, cols> ... " print
	if ( 
		(matrixA cols == matrixB rows) and
		(matrixA rows == matrixB cols),
		"ok" println,
		"failed" println
		return)
	"Checking matrix elements ... " print

	for (i, 0, matrixA rows-1,
		for (j, 0, matrixA cols-1,
			if (matrixA get(i,j) != matrixB get(j,i),
				"failed" println
				return)))
	"ok" println) 

transposeVerifier(data, data transpose)

"Task #7: Write matrix to file, and read it from file." println

List2D writeToFile := method(file,
	# file format:
	# "<row count>,<col count>" at first line
	# following by each elements taking up a line
	file remove
	file openForUpdating
	file write(rows asString, ",", cols asString, "\n")
	for (i, 0, rows-1,
		for (j, 0, cols-1,
			file write(get(i,j) asString, "\n")))
	file close)

List2D readFromFile := method(file,
	# currently only the raw string is stored
	# in the corresponding position
	file openForReading
	rows_cols := file readLine split(",")
	rows := rows_cols at(0) asNumber
	cols := rows_cols at(1) asNumber
	target := dim(rows, cols)
	for (i, 0, rows-1,
		for (j, 0, cols-1,
			target set(i,j, file readLine)))

	file close
	return target)

# please make sure not to put any important things
#     in file 'day-2-do-matrix.txt'

fileName := "day-2-do-matrix.txt"
file := File with(fileName)
data writeToFile(file)

rawData := List2D readFromFile(file)
"Reading done, print matrix:" println
rawData println

fileReadWriteVerifier := method(originMatrix, loadedMatrix,
	"Checking rows and cols ... " print
	if (
		(originMatrix cols != loadedMatrix cols) or
		(originMatrix rows != loadedMatrix rows),
		"failed" println
		return,
		"ok" println)
	"Checking elements in the matrix ... " print
	for (i, 0, originMatrix rows-1,
		for (j, 0, originMatrix cols-1,
			# because only the string version of the original data is loaded
			# we can only tell if the loaded data is identical to "(original data) asString"
			if ( originMatrix get(i,j) asString != loadedMatrix get(i,j),
				"failed" println
				return)))
	"ok" println
	"Check done." println)

fileReadWriteVerifier(data, rawData)

write("This task is done, removing '", fileName, "' ... ")
file remove
"Removed" println

"Task #8: Guess numbers" println

# some hints about this task:
#     most information will be found at 
#     http://iolanguage.org/scm/io/docs/reference/index.html
# * use 'Random' to generate a random number
# * use 'File standardInput' if you want some user interaction
# * you can pass an argument to 'readLine' as a prompt,
#     otherwise 'readLine' will print "nil" which is undesirable

GuessNumberGame := Object clone

GuessNumberGame newGame := method(retryTime,
	if (retryTime == nil,
		retryTime = 10)
	game := GuessNumberGame clone
	game restRetryTime := retryTime
	# will not use "Random value(1,100)" here
	# for which the occurrence of '100' will be lower than expected
	game targetNumber := (Random bytes(1) at(0) % 100) + 1 # 1 - 100
	return game)

GuessNumberGame play := method(
	"==== Game start ====" println
	userInput := File standardInput
	while( restRetryTime > 0,
		writeln( "Rest retry time: ", restRetryTime )
		restRetryTime = restRetryTime-1
		guess := userInput readLine("Guess a number between [1,100]: ") asNumber
		if (guess == targetNumber,
			"You got it!" println
			break,
			if (restRetryTime == 0,
				"No more retry time, the target number is " print
				targetNumber println,
				writeln(
					"Try again, the number is ",
					if (targetNumber > guess,
						"greater",
						"smaller"),
					" than you've guessed.")))
	)
	userInput close
	"==== Game over ====" println
)

GuessNumberGame newGame play
