#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	c:c("./modules/day-3/do/translate_server/translator"),
	c:c("./modules/day-3/do/translate_server/translate_service"),
	c:c("./modules/day-3/do/translate_server/translate_tcp"),
	c:c("./modules/day-3/do/translate_server/translate_supervisor"),

	{ok, S} = translate_supervisor:start_link(8103),

	u:ws(">>> Now simulate crashes"),
	translate_service:crash(translate_supervisor:get_service(S)),
	translate_tcp:stop(translate_supervisor:get_tcp_service(S)),
	timer:sleep(100),
	u:ws("<<< Tests done"),
	user_interaction(),
	u:ws(">>> Services stopping"),
	translate_supervisor:stop(S),
	timer:sleep(100),
	u:ws(">>> Services stopped"),
	ok.

user_interaction() ->
	u:ws("Enter 'quit' to quit this program:"),
	case io:fread("", "~s") of
		{ok, ["quit"]}
			-> ok;
		_
			-> user_interaction()
	end.
