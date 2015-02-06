Some special cases appears more often, and we can build-in optimizations for the evaluator.
I think this is totally fine. For example, a function application with its operator
being a symbol is a very common case, therefore the specialized optimization can be justified.

But not all specialized optimizations are justified to be put into the evaluator,
As they might complicate the implementation of the evaluator.
Moreover, in general we can perform more aggressive and complex optimization strategies
on compiler because compilation is a one-shot action - it only need to be performed once.
For evaluators, we need to interpret unknown expressions at runtime, and if there are optimizations
for evaluators, we should require them to be at least fast and efficient enough comparing with
the original code, which is not always possible.
