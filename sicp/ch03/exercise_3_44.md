Louis is wrong. This `transfer` is different from `exchange`.

I believe the essential difference is that the amount of money `transfer` transfers is constant,
while the amount of money `exchange` transfers depends on the bank account itself.

`transfer`'s behavior does not vary when the state of bank account (here is the balance) changes.
As long as the processor fully performs `transfer`, there is always some constant amount of money transferred.
But the different thing happened when we are trying to use `exchange`, which depends on the state
of the bank account (again, it's the balance): 

TODO: FIXME:
(Wait, there must be something wrong with my answer here.)
