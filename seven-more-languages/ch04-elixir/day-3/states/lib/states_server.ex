defmodule States.Server do
  use GenServer
  require VidStore

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
    new_state = Keyword.put(videos,item,new_video)
    GenServer.cast :video_store_backup, {:write,new_state}
    { :reply, new_video, new_state }
  end

  # handle_cast is a way to communicate with the server when
  # we don't need a response from it
  # here we only handle a very specific case,
  # which is adding an video to the store.
  def handle_cast({:add, video}, videos) do
    # adding one new video item
    new_state = [video|videos]
    GenServer.cast :video_store_backup, {:write,new_state}
    { :noreply, new_state }
  end
end

# TODO: will be a backup server that duplicates the state
# of our main video store server
defmodule States.ServerBackup do
  use GenServer
  require VidStore

  def start_link(state) do
    GenServer.start_link(__MODULE__, state, name: :video_store_backup)
  end

  def init(state) do
    # we could give it a :nothing
    # or alternatively {:just, <custom-initial-state>}
    { :ok, state }
  end

  def handle_cast({:write,state}, _) do
    # getting new state
    # and store it in a Maybe-thing
    { :noreply, {:just, state} }
  end

  # note that it returns either :nothing or {:just, videos}
  def handle_call(:read, _from, state) do
    { :reply, state, state }
  end

end
