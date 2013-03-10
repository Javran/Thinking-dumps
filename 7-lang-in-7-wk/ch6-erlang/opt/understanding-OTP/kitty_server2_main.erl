#!/usr/bin/env escript

print_cat(C) ->
	io:format("~p~n", [C]).

main(_) -> 
	c:c(my_server),
	c:c(kitty_server2),
	Pid = kitty_server2:start_link(),

	Cat1 = kitty_server2:order_cat(Pid, carl, brown, "loves to burn bridges"),
	print_cat(Cat1),
	kitty_server2:return_cat(Pid, Cat1),

	Cat2 = kitty_server2:order_cat(Pid, jimmy, orange, "cuddly"),
	print_cat(Cat2),

	Cat3 = kitty_server2:order_cat(Pid, jimmy, orange, "cuddly"),
	print_cat(Cat3),

	kitty_server2:return_cat(Pid, Cat3),
	kitty_server2:return_cat(Pid, Cat2),
	kitty_server2:close_shop(Pid),
	timer:sleep(100),
	io:format("Verifying termination ... ~p~n", [is_process_alive(Pid) == false]),
	ok.
