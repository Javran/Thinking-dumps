In the following table, the meaning of last 3 columns are:

* `env`: the necessity of saving and restoring `env` register
  **around the evaluation of the operator**
* `argl`: the necessity of saving and restoring `argl` register
  **around the evaluation of each operand**
* `proc`: the necessity of saving and restoring `proc` register
  **around the evaluation of the operand sequence**

Expression | `env` | `argl` | `proc`
--- | --- | --- | ----
`(f 'x 'y)`     | superfluous | 
`((f) 'x 'y)`   | superfluous |
`(f (g 'x) y)`  | superfluous |
`(f (g 'x) 'y)` | superfluous |

