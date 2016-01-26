# Question 1

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

# Question 2

I could not make sense of this question, so the following parts might be totally wrong.

The second question asks us to add a protocol so that state machines are forced
to have a `state` field. Here I could not understand what is a `field`, the following one
is the closest I can have:

```elixir
defprotocol StateField do
    def state( _ )
end
```

By enforcing VideoStore or VidStore of implementing this method,
we no longer need to call `state_machine`, and instead we call something like
`StateField.state( x )`. But what could this `x` be?
My guess is it should be an instance of VideoStore or VidStore.
But it doesn't make sense: why do we need to create an instance that serves nothing
but for Elixir protocol to determine what implementation should be called?
