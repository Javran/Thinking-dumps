(load "../common/utils.scm")
(load "../common/test-utils.scm")

#|
order of events matters.

imagine a sequence of events of withdrawing money from bank:
(here I assume the balance is sufficient for simplicity)

1. access balance
2. new-balance = balance - amount
3. set! balance to new-balance

now Peter and Paul are sharing a bank account, and they attempt
to withdraw some money from bank, simultaneously.

now, denote A for Peter, B for Paul

A1. access balance
A2. new-balance = balance - 10
A3. set! balance to balance - 10

B1. access balance
B2. new-balance = balance - 25
B3. set! balance to balance - 25

as long as things happens in order (A1,A2,A3) and (B1,B2,B3),
the system will not detect any problem.

however:

if things happens in the following order (which is vaild
in the perspective of the bank system):

A1. access balance = 100
B1. access balance = 100
A2. new-balance = 100 - 10 = 90
B2. new-balance = 100 - 25 = 75
A3. set! balance to 90
B3. set! balance to 75

the final balance will be 75, which is incorrect.

|#

(end-script)
