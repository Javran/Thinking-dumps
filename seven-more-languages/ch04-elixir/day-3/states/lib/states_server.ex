defmodule States.Server do
  use GenServer
  require VidStore

  def start_link(videos) do
    GenServer.start_link(__MODULE__, videos, name: :video_store)
  end

  def init(videos) do
    { :ok, videos }
  end

  def handle_call({action, item}, _from, videos) do
    video = videos[item]
    new_video = apply VidStore, action, [video]
    { :reply, new_video, Keyword.put(videos,item,new_video) }
  end

  def handle_cast({:add, video}, videos) do
    { :noreply, [video|videos] }
  end
end
