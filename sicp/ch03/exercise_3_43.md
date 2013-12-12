Suppose that the balances in three accounts start out as $10, $20, and $30,
and that multiple processes run, exchanging the balances in the accounts.

*Argue that if the processes are run sequentially, after any number of concurrent exchanges, the
account balances should be $10, $20, and $30 in some order.*

All the concurrent exchanges can be modelled as a program that accepts a vector of bank accounts
and returns a vector of bank accounts whose balances are modified version.
Making exchange is basically modifing two of these bank accounts and remain other accounts intact.

Now observe that for any of these exchanging program, the only difference between input and output
is that the balances of exactly two bank accounts are swapped. So an exchanging program is nothing more than
doing a permutation on bank accounts' balance.
If these programs run sequentially,
then we are actually performing permutation on bank accounts.
The result must be $10, $20, $30 in some order.

*Draw a timing diagram like the one in Figure 3.29 to show
how this condition can be violated if the exchanges are implemented
using the first version of the account-exchange program in this section.*

    (define (exchange account1 account2)
      (let ((difference (- (account1 ’balance)
                           (account2 ’balance))))
        ((account1 ’withdraw) difference)
        ((account2 ’deposit) difference)))

* Peter and Paul are sharing 3 accounts: `acc1`, `acc2`, `acc3`.
* Initialize balance: `acc1,acc2,acc3 => 10,20,30`
* Peter will exchange `acc2` and `acc1`.
* Paul will exchange `acc3` and `acc1`.
* Peter calculates `difference = 20-10 = 10`
* Paul calculates `difference = 30-10 = 20`
* Peter sets `acc2` to `20-10=10`
* Peter sets `acc1` to `10+10=20`
* Paul sets `acc3` to `30-20=10`
* Paul sets `acc1` to `20+20=40`
* Finally, the balance for each account: `acc1, acc2, acc3 => 40,10,10` (Wrong)

*On the other hand, argue that even with this exchange program, the
sum of the balances in the accounts will be preserved.*

We still use the previous example here.
The sum of the balances holds because the `difference` is calculated and stored
in the environment. And after that `difference` will be a constant.
We can guaranteed that all instructions in `exchange` will be executed. And exactly
one `withdraw` and one `deposit` is performed. So the amount of money is guaranteed
to be intact because no matter how many accounts are being operated.
The result of executing all the instructions
will be equivalent to a result of some sequential instructions. As every pair of instruction
(i.e. `withdraw` and `deposit`) will guarantee the sum does not change in the result,
The sum of the balances will not change.

*Draw a timing diagram to show how even this condition would be violated if
we did not serialize the transactions on individual accounts.*

TODO
