Starting from MIT/GNU Scheme Release 9.2,
they has added an awesome fix to make `load` no longer work
for file path who is prefixed `"./"`

If you spot error messages like the following one:

    ;  Loading "./something.scm"...
    ;The object here, passed as an argument to open-input-string, is not a string.
    ;To continue, call RESTART with an option number:
    ; (RESTART 1) => Return to read-eval-print level 1.

Please try to remove the leading `"./"`
(e.g. change `(load "./something.scm")`
to `(load "something.scm")`) and try again.
