The deadlock-avoidance mechanism does not work
if we cannot apply an ordering over resources.

For example, suppose two threads wants to exchange
the balances of two accounts, and in addition,
the accounts to exchange are only determined in runtime
(i.e. by passing argument). We need to keep logs
whenever we operate on an account.

Now, the first thread are given `acc1, acc2`
and the second thread are given `acc2, acc1`.
The ordering of acquiring resource cannot be applied here
because we need to do the task in order and record operations in log
and enforcing the ordering will lead to wrong log output.

Deadlock will happen if
the first thread holds `acc1` and is waiting on `acc2`
and
the second thread holds `acc2` and is waiting on `acc1`.
