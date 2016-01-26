ball_glove = fn -> receive do
  {:pitch, pitcher} ->
      send pitcher, {:catch, self()}
  after
    1_000 -> IO.puts "Nothing received"
  end
end

# spawn catcher
catcher = spawn ball_glove

# without sending anything
receive do
  {:catch, _} ->
    IO.puts "Caught it!"
after
  1_500 -> IO.puts "Nothing caught"
end

# the catcher waits for 1s while we wait 1.5s in main process
# to make sure catcher is timed out

# we send the message again, but I think the process is already done
send catcher, {:pitch, self()}

receive do
  {:catch, _} ->
    IO.puts "Caught it!"
after
  1_000 -> IO.puts "Nothing caught"
end

# nothing should be received
