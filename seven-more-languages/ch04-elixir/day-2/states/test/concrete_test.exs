defmodule ConcreteTest do
  use ExUnit.Case
  import Should
  should "update count" do
    rented_video = VideoStore.renting(video)
    assert rented_video.times_rented == 1
  end

  def video, do: %Video{title: "XMen"}
end
