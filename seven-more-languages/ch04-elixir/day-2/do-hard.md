Talking about `before_(event_name)` and `after_(event_name)`,
I'm not sure if I can follow: `StateMachine.Behavior` checks the transition table
of the state machine, changes the state and fires callbacks in order.
To maintain a better modularity, `StateMachine.Behavior` should minimize the assumption
made about a module thus it only looks at the transition table and not the whole module.
So it does not make any sense generating those functions and ask it to check their existence
in a module.

So here I have implemented hooks taking a different approach:
we specify them in the transition table,
and extend `StateMachine.Behavior` to check the existence of `before_calls` and `after_calls`.
