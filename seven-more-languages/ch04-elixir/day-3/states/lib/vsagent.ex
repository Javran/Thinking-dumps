defmodule VSAgent do
  require Agent
  
  def start_link(state) do
    Agent.start_link(fn -> state end, name: :vs_agent)
  end

end
