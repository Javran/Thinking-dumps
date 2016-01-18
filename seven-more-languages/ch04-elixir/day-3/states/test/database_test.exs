defmodule DatabaseTest do
  use ExUnit.Case

  test "trivial" do
    result = GenServer.cast :video_store, {:add, {:xmen, %Video{title: "X men"}}}
    assert result == :ok
  end

  test "break into parts? -- we can't do this" do
    result = GenServer.call :video_store, {:rent, :xmen}
    IO.puts (inspect result)
  end

end
