#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	c:c("./modules/day-3/do/logger"),

	LogFileName = "messages.out",

	% remove log file to keep it clean
	file:delete(LogFileName),

	{ok, Pid} = logger:start_link(LogFileName),

	lists:foreach(
		fun(W) ->
			logger:post(Pid,W)
		end,
		string:tokens("The quick brown fox jumps over the lazy dog"," ")),
	logger:stop(Pid),

	u:ws("Verify and output results:"),
	Lines = u:readlines(LogFileName),
	u:ws(string:join(Lines, "-")),	

	ok.
