#!/usr/bin/env escript

main(_) -> 
	c:c(u),
	u:ws("Fibs:"),
	Fibs = [1,1,2,3,5,8,13],
	u:wl(Fibs),

	u:ws("Double:"),
	Double = fun(X) -> X*2 end,
	u:wl(lists:map(Double, Fibs)),
	u:wl([Double(X) || X <-Fibs]),
	u:wl([X*2 || X <-Fibs]),

	u:ws("Cart:"),
	Cart = [{pencil, 4, 0.25}, {pen, 1, 1.20}, {paper, 2, 0.20}],
	u:wl(Cart),
	
	u:ws("WithTax:"),
	u:wl([{Product, Quantity, Price, Price * Quantity * 0.08} ||
			{Product, Quantity, Price} <- Cart ]),

	u:ws("DiscountedCat:"),
	u:wl([{Product, Price/2} || {Product, _, Price} <- Cart]),

	u:ws("More about list comprehension:"),
	u:wl([X || X <- [1,2,3,4], X < 4, X > 1]),
	% [2,3]
	u:wl([{X,Y} || X <- [1,2,3,4], X < 3, Y <-[5,6]]),
	% {[12],[56]} (regex)
	ok.
