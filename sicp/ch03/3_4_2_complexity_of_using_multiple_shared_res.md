Serializers can handle the case that single resource 
is shared and used amount multiple concurrent programs.

But as the number of resources shared grows up, the complexity
grows up as well.

If we have 3 bank accounts with balance $10, $20, $30 shared amount some
concurrent programs and each program does nothing but exchanging all balances
between any of two accounts. The result must be $10, $20, $30 in some order right after
any concurrent program finished its execution. But per-account-serializer does not guaranteed this.

One way of solving this problem is to use all serializers from each accounts.
The drawback is: we have to expose the serializer and make sure serializers are
called explicitly or we might run into troubles.

P.S.: In addition, we should make sure the serializers are called
in some constant order or we might be a victim of deadlock.
(e.g. `program-1` calls `serializer-1` first, then `serializer-2`, while `program-2`
calls first `serializer-2`, followed by calling `serializer-1`. In this scenario,
`program-1` holds `serializer-1` and is waiting on `serializer-2` whereas `program-2`
holds `serializer-2` and is waiting on `serializer-1`.)
