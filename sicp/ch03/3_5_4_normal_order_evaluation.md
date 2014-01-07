recall there are two evaluation methods:

* applicative order: "evaluate the arguments and then apply"
the procedure continues only if all its arguments get evaluated.
* normal order: "fully expand and then reduce"
arguments are evaluated as needed. (e.g. `delay` and `force`)
