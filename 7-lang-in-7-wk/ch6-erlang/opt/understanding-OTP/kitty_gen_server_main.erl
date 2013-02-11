#!/usr/bin/env escript

print_cat(C) ->
	io:format("~p~n", [C]).

main(_) -> 
	c:c(kitty_gen_server),
	{ok, Pid} = kitty_gen_server:start_link(),

	Pid ! <<"Test handle_info">>,

	Cat1 = kitty_gen_server:order_cat(Pid, "Cat Stevens", white, "not actually a cat"),
	print_cat(Cat1),
	kitty_gen_server:return_cat(Pid, Cat1),

	Cat2 = kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!"),
	print_cat(Cat2),

	Cat3 = kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!"),
	print_cat(Cat3),

	kitty_gen_server:return_cat(Pid, Cat3),
	kitty_gen_server:return_cat(Pid, Cat2),
	kitty_gen_server:close_shop(Pid),
	timer:sleep(100),
	io:format("Verifying termination ... ~p~n", [is_process_alive(Pid) == false]),
	ok.
