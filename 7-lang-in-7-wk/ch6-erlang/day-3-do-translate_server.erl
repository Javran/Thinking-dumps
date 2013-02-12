#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	c:c("./modules/day-3/do/translate_server/translator"),
	c:c("./modules/day-3/do/translate_server/translate_service"),

	{ok, S} = translate_service:start_link(),

	ok.
