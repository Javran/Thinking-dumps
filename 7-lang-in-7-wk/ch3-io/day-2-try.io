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

# the expression is actually parsed into:
testXor( "true xor(true)" ) # false

"==== Now, let's talk about messages ====" println

postOffice := Object clone
postOffice packageSender := method(call sender)

mailer := Object clone
mailer deliver := method(postOffice packageSender)

" fire slot 'deliver' ..." println
mailer deliver println
# what happened here?
# 1. slot 'deliver' was fired(sent to 'mailer')
# 2. a message 'packageSender' was sent to 'postOffice'
# 3. slot 'packageSender' was fired
# 4. 'call' fetched the sender, which is 'deliver'
# 5. information about 'deliver' was printed

" try to send 'slotNames' to 'postOffice'" println
mailer fireSlotNamesOf := method( recipient,
	recipient slotNames)

mailer fireSlotNamesOf(postOffice) println

" add slot 'messageTarget' for 'postOffice' ..." println
postOffice messageTarget := method(call target)
" fire the slot ..." println
postOffice messageTarget println
# what happend here?
# 1. slot 'messageTarget' was sent to postOffice, and fired
# 2. the message's target 'postOffice' was fetched and printed

" add slot 'messageArgs','messageName' for 'postOffice' ..." println
postOffice messageArgs := method(call message arguments)
postOffice messageName := method(call message name)

# what the heck is ':three' ?
# seems any thing here is acceptable .. 
# thus io language uses lazy evaluation
postOffice messageArgs("one", 2, %^&%$^$& three) println
postOffice messageName println

# given it's ok to assign methods without specifying a host object
# I want to know who is this method's host
whoareyou := method(call target)
whoareyou println

# you'll be able to see 'postOffice', 'mailer', 'whoareyou', etc.
# so there are hosted to a special Object

messageViewer := method(
	"===== message sender:" println
	call sender println
	"===== message target:" println
	call target println
	"===== message name:" println
	call message name println
	"===== message arguments:" println
	call message arguments println)

"try to send 'messageViewer' from 'postOffice' to 'mailer'"
postOffice fireViewer := method(mailer messageViewer(1,2,3))
postOffice fireViewer
