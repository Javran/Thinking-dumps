defmodule VSAgentTest do
  use ExUnit.Case

  require VSAgent

  setup do
    {:ok, pid} = VSAgent.start_link([])



    {:ok, [pid: pid]}
  end

end
