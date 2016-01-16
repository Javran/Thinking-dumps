## Question 1

* How can you crash your server?

    The server can be crashed by either:

    - running codes that might lead to error on server side
    - sending a message that cannot be matched by handler function

* What happens when you crash it with and without a supervisor?

With a supervisor, the process being supervised is restarted.
Depending on the strategy used, some other process might be terminated or restarted as well.
Check [Supervisor document](http://elixir-lang.org/docs/v1.0/elixir/Supervisor.html) for detail.

Without a supervisor, when process crashes, it crashes. After all no one is responsible for
monitoring this process and restart it when necessary.
