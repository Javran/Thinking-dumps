Expression | `env` | `proc` | `argl`
--- | --- | --- | ----
`(f 'x 'y)` | superfluous | superfluous | superfluous
`((f) 'x 'y)` | | |
`(f (g 'x) y)` | | |
`(f (g 'x) 'y)` | | |
