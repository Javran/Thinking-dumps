#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	Negate = fun(I) -> -I end,
	u:wl( Negate(1) ),
	% -1
	u:wl( Negate(0) ),
	% 0
	u:wl( Negate(-1) ),
	% 1

	Sign = fun(I) ->
			if
				I > 0 -> 1;
				I < 0 -> -1;
				true -> 0
			end
		end,
	
	u:wl( Sign( 123.45 ) ),
	% 1
	u:wl( Sign( 0.5 - 1/2 ) ),
	% 0
	u:wl( Sign( -1234 ) ),
	% -1
	ok.
