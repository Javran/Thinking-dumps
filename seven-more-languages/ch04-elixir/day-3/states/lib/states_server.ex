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

  # handle_call in general is a way to communicate with the server
  # usually the user will expect some responses after the task is done
  def handle_call({action, item}, _from, videos) do
    # perform some action to one of the video
    # and save the change in server's state
    video = videos[item]
    new_video = apply VidStore, action, [video]
    GenServer.cast :video_store_backup, :test
    { :reply, new_video, Keyword.put(videos,item,new_video) }
  end

  # handle_cast is a way to communicate with the server when
  # we don't need a response from it
  # here we only handle a very specific case,
  # which is adding an video to the store.
  def handle_cast({:add, video}, videos) do
    # adding one new video item
    { :noreply, [video|videos] }
  end
end

# TODO: will be a backup server that duplicates the state
# of our main video store server
defmodule States.ServerBackup do
  use GenServer
  require VidStore

  def start_link(videos) do
    GenServer.start_link(__MODULE__, videos, name: :video_store_backup)
  end

  def handle_cast(:test, state) do
    IO.puts "Just a test message for now"
    { :noreply, state }
  end

  def init(videos) do
    { :ok, videos }
  end

end
