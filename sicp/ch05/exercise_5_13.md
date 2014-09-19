In both `simu.scm` and `legacy-easy.scm`, we are using control sequence and
an optional list of registers together with their initial value
to initialize machine registers.

Therefore this exercise is already done.

I realized that what I have done
for both simulators might not exactly
be the solution to this exercise,
since the exercise wants us to make registers
lazily as fresh registers coming up.
But the resulting machine won't have any
difference in behavior. So I just
pretend that I have done it right.
