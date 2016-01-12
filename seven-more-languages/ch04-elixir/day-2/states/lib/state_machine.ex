defmodule StateMachine do
  # triggered when "use StateMachine" is used
  defmacro __using__(_) do
    quote do
      import StateMachine
      @states []
      @before_compile StateMachine

      # one interesting thing is that:
      # if we print anything here,
      # the message will be printed after declaration generated by "state" function
      # which makes sense:
      # what we are really doing is just some compile-time AST manipulation,
      # and the full code should be executed after marco expansion step
      
      # uncomment the following line for example
      # IO.puts "using StateMachine!"
    end
  end

  defmacro state(name, events) do
    quote do
      @states [{unquote(name), unquote(events)} | @states]
    end
  end

  # after marco subsitution but before compilation
  # like a post-processing step
  defmacro __before_compile__(env) do
    # module attribute is used to exchange data?
    states = Module.get_attribute(env.module, :states)
    # get all events from state declaration
    events = states
      |> Keyword.values
      |> List.flatten
      |> Keyword.keys
      |> Enum.uniq

    quote do
        def state_machine do
          unquote(states)
        end

        unquote event_callbacks(events)
    end
  end

  def event_callback(name) do
    callback = name
    quote do
      # I believe it means (unquote(name))(context)
      # which defines the function for each event name
      def unquote(name)(context) do
        StateMachine.Behavior.fire( state_machine, context, unquote(callback) )
      end

      # TODO: we could have generated before_name, after_name callbacks here
      # but then who should be responsible for calling them?
    end
  end

  def event_callbacks(names) do
    Enum.map(names, &event_callback/1)
  end
end
