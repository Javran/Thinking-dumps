## Task #1: monitor the translate_service, restart it if it dies

Please refer to `day-3-do-translate_service_restart.erl`, which ask the service to stop, 
and use the service again when it was brought back by monitor.

There's another version of answer: `day-3-do-translate_service_restart_otp.erl`, which is compatible with OTP,
takes advantage of [supervisor](http://www.erlang.org/doc/man/supervisor.html) to do the trick.

## Task #2: make Doctor restart itself if it dies

Although `trap_exit` flag make it possible to catch exiting message,
I don't know how can a dead process restart itself.
Maybe it's possble, but it is not a recommended practice.
Skip this task.

Please refer to: [Erlang process monitoring itself](http://stackoverflow.com/questions/8464369/erlang-process-monitoring-itself).

## Task #3: write a monitor to monitor Doctor, if either Doctor or monitor dies, restart it.

Please refer to `day-3-do-monitored-doctor.erl`

## Task #4: build an OTP server that can write messages to files

Please refer to `day-3-do-logger.erl`

## Task #5: make `translate_service` avaliable through network

Please refer to `day-3-do-translate_server`

How to use:

* Run `day-3-do-translate_server` first, wait until all tests done(an error will be reported) 
and the program prints `Enter 'quit' to quit this program:`

* Open another terminal/console, use `telnet` to access the service, the program uses `127.0.0.1:8103`

* you can also find `day-3-do-translate_server_result.txt`, where I've put some run results.
