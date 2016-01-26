defmodule VSAgentTest do
  use ExUnit.Case

  require VSAgent

  setup do
    VSAgent.start_link([])
    install_database()
    :ok
  end

  def install_database() do
    VSAgent.cast {:add, {:wolverine, %Video{title: "Wolverine"}}}
    VSAgent.cast {:add, {:xmen, %Video{title: "X men"}}}
    VSAgent.cast {:add, {:et, %Video{title: "ET"}}}
  end

  test "rent all", _ do
    VSAgent.call {:rent, :wolverine}
    VSAgent.call {:rent, :xmen}
    VSAgent.call {:rent, :et}
  end

  test "rent multiple times", _ do
    VSAgent.call {:rent, :wolverine}
    VSAgent.call {:rent, :xmen}
    VSAgent.call {:return, :wolverine}

    VSAgent.call {:rent, :wolverine}
    VSAgent.call {:return, :xmen}
    result_w = VSAgent.call {:return, :wolverine}
    assert 2 == result_w.times_rented

    result_x = VSAgent.call {:return, :xmen}
    assert 1 == result_x.times_rented
  end

end
