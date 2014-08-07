# Instruction Summary

* `assign`

    * `(assign <register-name> (reg <register-name>))`
    * `(assign <register-name> (const <constant-value>))`
    * `(assign <register-name> (op <opeartion-name>) <input1> <input2> ...)`
    * `(assign <register-name> (label <label-name>))`

* `perform`

    * `(perform (op <opeartion-name>) <input1> <input2> ...)`

* `test`

    * `(test (op <opeartion-name>) <input1> <input2> ...)`

* `branch`

    * `(branch (label <label-name>))`
        * I think enabling `reg` support for `branch` should be easy. But anyway.

* `goto`

    * `(goto (label <label-name>))`
    * `(goto (reg <register-name>))`

* `save` and `restore`

    * `(save <register-name>)`
    * `(restore <register-name>)`

* `const`

    * `(const <data>)`
        * I think there's no limit no what kind of data it can store:
          string, symbol, list or even quoted s-exp.
