#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	c:c("./modules/day-3/try/translate_service"),

	Translator = spawn(fun translate_service:loop/0),

	translate_service:translate(Translator, "blanca"),
	translate_service:translate(Translator, "casa"),
	
	Translator ! {self(), stop},
	receive
		stopped -> u:ws("<Translator stopped>")
	end.
