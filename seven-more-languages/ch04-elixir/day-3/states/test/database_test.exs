defmodule DatabaseTest do
  use ExUnit.Case

  setup do
    GenServer.cast :video_store, {:add, {:wolverine, %Video{title: "Wolverine"}}}
    GenServer.cast :video_store, {:add, {:xmen, %Video{title: "X men"}}}
    GenServer.cast :video_store, {:add, {:et, %Video{title: "ET"}}}
    :ok
  end

  test "rent all", _ do
    # try to rent all videos and see if there are any errors
    GenServer.call :video_store, {:rent, :wolverine}
    GenServer.call :video_store, {:rent, :xmen}
    GenServer.call :video_store, {:rent, :et}

  end

  test "rent multiple times", _ do
    GenServer.call :video_store, {:rent, :wolverine}
    GenServer.call :video_store, {:rent, :xmen}
    GenServer.call :video_store, {:return, :wolverine}

    GenServer.call :video_store, {:rent, :wolverine}
    GenServer.call :video_store, {:return, :xmen}
    result_w = GenServer.call :video_store, {:return, :wolverine}
    assert 2 == result_w.times_rented

    result_x = GenServer.call :video_store, {:return, :xmen}
    assert 1 == result_x.times_rented
  end

end
