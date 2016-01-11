The modification is made directly on code example given in the book.
I have also added few testcases to confirm that both of them are working.

Short answer: modifying the abstract one is easier.

Long answer:

This is not a simple pick-one-from-two question. To few pros and cons:

* The concrete one is more straightforward, but more boilerplate are needed.
* The abstract version (with metaprogramming) needs less modification, but
  there are subtle issues (for example, we cannot name two function when the same name
  `found`). But most of the time we only occasionally run into these problems,
  and the problem can be easily found from reading warnings and error messages.
