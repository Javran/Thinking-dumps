Louis's suggestion might lead to deadlock.
We need to guarantee that the resource
(here the resources are bank accounts)
does not wait on some resource
that has already been acquired by other sleeping threads.

Suppose Peter and Paul shares bank account `acc1`, `acc2`.

Peter want to perform `(serialized-exchange acc1 acc2)` while 
Paul want to perform `(serialized-exchange acc2 acc1)`.

(Yes it makes no sense in real world. But here
these operations do not violate assumptions that the implementation
has made, plus, I need a simple scenario to point out the problem.)

Now Peter uses serializer of `acc1` to acquire 
an exclusive ownership of `acc1`.

In the meantime, Paul uses serializer of `acc2` to acquire
an exclusive ownership of `acc2`.

The problem comes when Peter holds `acc1` and waits for
the exclusive ownership of `acc2`,
while Paul holds `acc2` and waits for
the exclusive ownership of `acc1`.

This two serializers are all waiting for the resource that have
already be acquried by other threads. Furthermore, neither of them
will release the resources' ownership.

This situation is a "deadlock" because the dependencies of the resources
forms a cycle, and the program will never terminate.

I think one way to solve this problem is that

* we still provide the serializer explicitly
* but this time, we make any two serializers comparable to each other
(this can be easily achieved by using something like unique bank account number)
* for any procedure that requires serialization, we use an auxiliary function to retrieve
all the serializers, and sort them to force a definite order of requiring resouces.
* call these serializers from this auxiliary function in the order given.
