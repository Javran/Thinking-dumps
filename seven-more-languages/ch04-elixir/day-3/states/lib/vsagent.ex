defmodule VSAgent do
  require Agent
  require VidStore
  
  def start_link(state) do
    Agent.start_link(fn -> state end, name: :vs_agent)
  end

  def call({action,item}) do
    Agent.get_and_update(
      :vs_agent,
      fn videos ->
        video = videos[item]
        new_video = apply VidStore, action, [video]
        new_state = Keyword.put(videos,item,new_video)
        { new_video, new_state }
      end)
  end

  def cast({:add,video}) do
    Agent.update(
      :vs_agent,
      fn videos -> [video|videos] end)
  end

end
