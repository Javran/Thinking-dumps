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
