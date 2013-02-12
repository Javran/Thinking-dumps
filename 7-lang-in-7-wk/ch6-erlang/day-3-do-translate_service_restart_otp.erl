#!/usr/bin/env escript

% Task #1 Extra: make translate_service OTP-compatible and 
%     use supervisor to achieve Task #1

main(_) ->
	c:c(u),
	c:c("./modules/day-3/do/translate_service_otp"),
	c:c("./modules/day-3/do/translate_service_monitor_otp"),

	translate_service_monitor_otp:start_link(),

	Translate = fun(OriginWord) ->
		io:format("Translate request: ~p~n", [OriginWord]),
		Word = translate_service_otp:translate(translator, OriginWord),
		io:format("Translation: ~p~n", [Word])
	end,

	Translate("casa"),
	% raise an error
	translate_service_otp:test_error(translator),
	% wait for a while until service brought back again
	timer:sleep(100),
	Translate("blanca"),
	ok.
