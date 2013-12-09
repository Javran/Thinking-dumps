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
