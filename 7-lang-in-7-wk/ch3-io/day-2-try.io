#!/usr/bin/env io

# it's an infinity loop
# loop("getting dizzy..." println)

# ';' will link two messages together
"while:" println
i := 1
while(i<=11, i println; i = i+1); 
	"This one goes up to 11" println

# seems 'x' need not to be assigned
"for:" println
for(x, 1, 11, x println);
	"This one goes up to 11" println

"for with increment:" println
for(x, 1, 11, 2, x println);
	"This one goes up to 11" println

"for with extra argument:" println
for(x, 1, 11, 2, x println, "this will never be printed" println)

if(true, "it is true", "it is false") println
# it is true

if(false, "it is true", "it is false") println
# it is false

if(true) \ 
	then("it is true" println) \
	else("it is false" println)
# it is true

if(false) \
	then("it is true" println) \
	else("it is false" println)
# it is false

"print OperatorTable:" println
OperatorTable println

"add new operator 'xor' ..." println
OperatorTable addOperator("xor", 11)
OperatorTable println

true xor := method(bool, if(bool, false, true))
false xor := method(bool, if(bool, true, false))

testXor := method(cmd,
	("eval string: " .. cmd) println
	doString(cmd) println)

testXor( "true xor true" ) 	# false
testXor( "false xor false" ) 	# false
testXor( "true xor false" ) 	# true
testXor( "false xor true" ) 	# true
