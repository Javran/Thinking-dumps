## Question 1: Backup Server

For this task we maintain two servers on different processes:
one provides user with full functionalities, and another backup.

Two servers are declared in the application so when the application starts,
both server can refer to the other one by symbols.

Whenever the main server gets updated, it sends its updated state to the backup server.
This saves us from having to redo everything on the backup server again in order to sync.

When main server starts, it first try to communicate with the backup server and retrieve
the last correct state.
This retrieval might fail due to:

- the backup server is not yet available (a fresh start)
- the backup server does not hold a state (a fresh start)

Both of the above can be recovered by falling back to the default state.

When the main server crashes, same communication happens. As long as there are updates
to the main server, backup server should have received it, so we can recover
from a known correct state
(And hopefully a crash to the main server does no harm to our backup server)

Unfortunately I don't know how to tell our test framework not to stop when a server crashes.
So to test the behavior after crashing main server, you will have to use Elixir REPL.

## Question 2: Use Agent

We can almost reuse the same interface our `GenSever` has.
The testcases under `states/test/vsagent_test.exs` shows how the user code looks like.
For the implementation, I find `Agent.get_and_update` to be a perfect primitive for this purpose.
See `states/lib/vsagent.ex` for detail.

## Question 3: Persist into Erlang's DETS database

(TODO) Related document: [dets](http://erlang.org/doc/man/dets.html).
In our case, we want to persist the whole state of main server into a file.
The main server state is a `Keyword`, with symbols being keys, object being the other column,
this task should be easy.

However, there is still one concern: it will be inefficient writing to files
for each modification made. What we could do is to allow changes to take some time
before being persisted in disk. One naive way is to keep a counter: for each one chance, the
counter bumps, and when there are sufficient bumps, the counter is reset and the current state is persisted.
This approach is only meant to be simple. I'm sure there are most
sophisticated technique for addressing this problem,
I don't know at this time though.
