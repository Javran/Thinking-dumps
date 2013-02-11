#!/usr/bin/env escript

% Task #1: monitor translate_service, restart it when it was terminated

translate(Word) ->
	translator ! {self(), Word},
	receive
		Translation ->
			u:ws("Translation:"),
			u:ws(Translation)
	end.

main(_) ->
	c:c(u),
	c:c("./modules/day-3/do/translate_service"),
	c:c("./modules/day-3/do/translate_service_monitor"),

	Monitor = spawn(fun translate_service_monitor:loop/0),
	Monitor ! new,
	timer:sleep(100),

	translate("casa"),
	translate("blanca"),

	translator ! stop,
	timer:sleep(100),

	translate("casa"),
	timer:sleep(100),

	ok.
