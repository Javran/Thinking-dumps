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
    [ available:
      [ rent:
        [ to: :rented,
          calls: [ &VideoStore.renting/1 ]
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
end
