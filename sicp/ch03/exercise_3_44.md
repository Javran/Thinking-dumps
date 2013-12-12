Louis is wrong. This `transfer` is different from `exchange`.

I believe the essential difference is that the amount of money `transfer` transfers is constant,
while the amount of money `exchange` transfers depends on the bank account itself.

`transfer`'s behavior does not vary when the state of bank account (here is the balance) changes.
As long as the processor fully performs `transfer`, there is always some constant amount of money transferred.
But the different thing happened when we are trying to use `exchange`, which depends on the state
of the bank account (again, it's the balance).

Still taking the first example from exercise 3.43
that Peter and Paul shares 3 bank accounts,
one of the correct way to do the exchange is:

* Peter and Paul are sharing 3 accounts: `acc1`, `acc2`, `acc3`.
* Initialize balance: `acc1,acc2,acc3 => 10,20,30`
* Peter will exchange `acc2` and `acc1`.
* Paul will exchange `acc3` and `acc1`.
* Peter calculates `difference = 20-10 = 10`
* Peter sets `acc2` to `20-10=10`
* Peter sets `acc1` to `10+10=20`
* Paul calculates `difference = 30-20 = 10`
* Paul sets `acc3` to `30-10=20`
* Paul sets `acc1` to `20+10=30`
* Finally, the balance for each account: `acc1, acc2, acc3 => 30,10,20` (Correct)

There one of either Peter and Paul must fully modify the variable he will affect
before the other one getting access to the bank balance.
Otherwise, the `difference` calculated in the middle of the operation will differ,
and it eventually leads to the violation of "permutation law".

