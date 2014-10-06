# A plan for the exercise

* Currently an instruction is represented
by the text of the instruction and a procedure
that actually performs the operation,
extend the representation to include the label right before it

    * change the structure of instruction representation and
    make sure it still works
    * add new extra fields while keeping the other information intact
    (might change the old representation to a list)

* The label is optional and must be the one right before it.
Think carefully about what will happen if there are more than
two labels right before an instruction (in this case I think the
later one wins according to the exercise requirement.

* Demonstrate that our approach works by bringing instruction
counting feature online. For legacy simulator there might be some
extra works to be done, as most of the time patches are
incompatible against each other.
