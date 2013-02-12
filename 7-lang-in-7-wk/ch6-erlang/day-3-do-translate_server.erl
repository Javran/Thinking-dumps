#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	c:c("./modules/day-3/do/translate_server/translator"),
	c:c("./modules/day-3/do/translate_server/translate_service"),
	c:c("./modules/day-3/do/translate_server/translate_supervisor"),

	{ok, S} = translate_supervisor:start_link(),
	Service = translate_supervisor:get_service(S),
	
	translate_service:crash(Service),

	timer:sleep(100),

	u:wl(translate_service:translate(translate_supervisor:get_service(S), "casa")),

	translate_service:stop( translate_supervisor:get_service(S) ),

	timer:sleep(100),
	

	ok.
