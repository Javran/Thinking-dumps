defmodule DatabaseTest do
  use ExUnit.Case

  setup do
    GenServer.cast :video_store, {:add, {:wolverine, %Video{title: "Wolverine"}}}
    GenServer.cast :video_store, {:add, {:xmen, %Video{title: "X men"}}}
    GenServer.cast :video_store, {:add, {:et, %Video{title: "ET"}}}
    :ok
  end

  # try to rent all videos and see if there are any errors
  test "rent all", _ do
    GenServer.call :video_store, {:rent, :wolverine}
    GenServer.call :video_store, {:rent, :xmen}
    GenServer.call :video_store, {:rent, :et}
  end

end
