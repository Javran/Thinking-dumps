Our task is to:

* Modify the compiler and change the target language to C

    As C is more expressive than a "bytecode" language,
    we should have no problem implementing a code generator.
    Work out some examples, namely some lisp code and what it should be compiled to.

* Compile the metacircular evaluator using the compiler

    We need to have some primitive operations to support this

* Execute the generated compiler, and test it

    Investigate if existing tests can be reused
