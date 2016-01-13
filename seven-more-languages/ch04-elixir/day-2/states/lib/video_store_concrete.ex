defmodule VideoStore.Concrete do import StateMachine.Behavior
  def rent(video), do:
    fire(state_machine, video, :rent)
  def return(video), do:
    fire(state_machine, video, :return)
  def lose(video), do:
    fire(state_machine, video, :lose)
  # added when doing exercise
  def found(video), do:
    fire(state_machine, video, :found)

  def state_machine do
    make_hook_with_message = fn (msg) ->
      fn (ctxt) ->
        IO.puts "hook function triggered"
        IO.puts "  message: #{inspect msg}"
        IO.puts "  context: #{inspect ctxt}"
      end
    end

    [ available:
      [ rent:
        [ to: :rented,
          before_calls: [ make_hook_with_message.("before_rent") ],
          calls: [ &VideoStore.renting/1 ],
          after_calls: [ make_hook_with_message.("after_rent") ]
        ]],
      rented:
      [ return:
        [ to: :available,
          calls: [ &VideoStore.returning/1 ]
        ],
        lose:
        [ to: :lost,
          calls: [ &VideoStore.losing/1 ]
        ]],
      lost:
      # added when doing exercise
      [ found:
        [ to: :available,
          calls: [ &VideoStore.found/1 ]
        ]]
    ]
  end

  import StateMachine
  defimpl StateMachine.StateField, for: VideoStore.Concrete do
    def state( vsc ), do: vsc.state_machine
  end
end
