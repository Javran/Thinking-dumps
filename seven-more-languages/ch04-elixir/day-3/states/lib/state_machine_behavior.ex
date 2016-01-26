defmodule StateMachine.Behavior do
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
