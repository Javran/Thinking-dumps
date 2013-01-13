#!/usr/bin/env io

Builder := Object clone

Builder forward := method(
	# method is a kind of message.
	# method name is message name
	# so body(...) will produce "<body>" here
	writeln("<", call message name, ">")

	# each argument will print the corresponding XML data.
	call message arguments foreach(arg,
		# this will call the 'forward' indirectly
		content := self doMessage(arg)
		if (content type == "Sequence", writeln(content)))
	# produce "</body>" here
	writeln("</", call message name, ">"))

Builder ul(
		# if the line below is uncommented, 
		#     the method will be found and 
		#     Builder itself will receive message "println"
		# println,
		li("Io"),
		li("Lua"),
		li("JavaScript"))
