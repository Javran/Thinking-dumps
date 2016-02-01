## Parallel Computing

* [Parallel Computing](http://docs.julialang.org/en/latest/manual/parallel-computing/)
* [`@spawn`](http://docs.julialang.org/en/latest/stdlib/parallel/#Base.@spawn)
* [`@everywhere`](http://docs.julialang.org/en/latest/stdlib/parallel/#Base.@everywhere)

I've tried to use them, see working code in `find.jl`.
One of the most important thing is: you need to make sure your function is
made available in the worker process. `@everywhere` is designed for this purpose.

## Wikipedia page on multiple dispatch

[Multiple dispatch](https://en.wikipedia.org/wiki/Multiple_dispatch)
