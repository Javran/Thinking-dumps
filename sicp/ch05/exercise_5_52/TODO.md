Our task is to:

* Modify the compiler and change the target language to C

    As C is more expressive than a "bytecode" language,
    we should have no problem implementing a code generator.
    Work out some examples, namely some lisp code and what it should be compiled to.

    2 parts are necessary for this interpreter:

    * Written code for runtime support

        Like what we did in ex 5.51, the interpreter will accept one file name,
        parse and interpret code from it. This part will be done in written code.

        This time we need an abstract machine. As we are generating sequence of statements in C,
        it is hard to think about how to implement lisp procedures in C.
        Instead we just generate C statements, which is a little closer to the original "bytecode"
        language.

    * Generated code that does the actual job

* Compile the metacircular evaluator using the compiler

    We need to have some primitive operations to support this

* Execute the generated compiler, and test it

    Investigate if existing tests can be reused
