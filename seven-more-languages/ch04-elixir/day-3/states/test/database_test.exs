defmodule DatabaseTest do
  use ExUnit.Case

  test "trivial" do
    result = GenServer.cast :video_store, {:add, {:xmen, %Video{title: "X men"}}}
    assert result == :ok
  end

end
