defmodule StateMachine.Behavior do
  # we assume the representation of state machines
  # is of the following structure:
  # - a Keyword
  #   - states as keys
  #   - Keywords as values
  #     - events as keys
  #     - Keywords as values
  #       - to: state after the event is fired
  #       - calls: callbacks to transform the context
  #         all calls accepts a context, return the transformed version of it
  #       - (extended) before_calls: unary functions that take a "transition tuple"
  #       - (extended) after_calls: unary function that take a "transition tuple"
  # (an example can be found in VideoStore.Concrete)
  # the last two are marked "extended", which is part of the exercise.
  # a "transition tuple" is defined as {before_state, event, after_state}
  def fire(context, event) do
    # NOTE: x |> f(a,b,c) is f(x,a,b,c)
    %{context | state: event[:to]}
    |> activate(event)
  end

  def fire(states, context, event_name) do
    event = states[context.state][event_name]
    fire(context, event)
  end

  def activate(context,event) do
    # fire all callback in order
    # and each callback is responsible for returning modified
    # context which will then be passed to the next callback function
    Enum.reduce(
      event[:calls] || [],
      context,
      &( &1.(&2) ))
  end
end
