## Question 1: Backup Server

The plan is: we implement a new server and run this server on a different process.
When the application is started, two processes will know each other.
As the main server gets updated, it sends its state to the backup server.
(This saves us from having to redo everything on the backup server again in order to sync)
And when the main server crashes, it will attempt to communicate with the backup server
(which hopefully is still up). If there is a state held by backup server, then
that state also becomes the initial state of the newly started main server.
