defmodule States.Server do
  use GenServer
  require VidStore

  # boilerplate?
  def start_link(videos) do
    GenServer.start_link(__MODULE__, videos, name: :video_store)
  end

  # initialize the server with a list of videos as the state
  def init(videos) do
    { :ok, videos }
  end

  # perform some action to one of the video
  # and save the change in server's state
  def handle_call({action, item}, _from, videos) do
    video = videos[item]
    new_video = apply VidStore, action, [video]
    { :reply, new_video, Keyword.put(videos,item,new_video) }
  end

  # adding one new video item
  def handle_cast({:add, video}, videos) do
    { :noreply, [video|videos] }
  end
end
