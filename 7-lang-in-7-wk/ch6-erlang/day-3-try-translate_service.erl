#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	code:add_patha("./day-3-modules"),
	c:c("./day-3-modules/translate_service"),

	Translator = spawn(fun translate_service:loop/0),

	translate_service:translate(Translator, "blanca"),
	translate_service:translate(Translator, "casa"),
	
	Translator ! {self(), stop},
	receive
		stopped -> u:ws("<Translator stopped>")
	end.
