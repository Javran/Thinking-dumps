The task has already been done in `./bank_account_exchange.rkt`,
here I will only demostrate why it can avoid deadlock.

If we draw directed edges:

* from the thread that is waiting for the resource, to the resource
* from the resource, to the current resource owner

and if:

* there is a cycle in the graph
* threads are waiting for each other
* there is no way to yield a resouce that has been owned

Then there must be a deadlock.

So in order to prevent this from happening,
we enforce an ordering over all serializers.

By doing this, no cycle will be formed.

To give a demonstration:

Assume resources are `r1, r2, r3, ...` and
the corresponding serializers are `s1, s2, s3, ...`
which meets the condition that `s1 < s2 < s3 < ...`

Now every thread can only require `s[j]` if it owns
`s[i]` and `i<j` (for simplicity, `s[0]` is an empty resource,
every thread holds it and owning it effects nothing)
and `s[i]` is the biggest among all resources that are owned by this thread.

Suppose `thread #1` owns `c3`, there is no way to acquire `c2` and `c1`,
but `thread #2` owns `c2` can wait on `c3`,
and the dependency cycle is no longer exist.

To enforce an ordering, we can simply assign an unique id for each
serializer, this is what we have achieved to do.
