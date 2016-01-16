# The Erlang `gen_server` Behaviors

[The Erlang `gen_server` Behaviors](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)

# The way to code a timeout with an Elixir receive

Use `after` keyword:

```elixir
iex> receive do
...>   {:hello, msg}  -> msg
...> after
...>   1_000 -> "nothing after 1s"
...> end
"nothing after 1s"
```

[Document can be found here](http://elixir-lang.org/getting-started/processes.html)

# Information on Erlang's OTP

* [Erlang/OTP 18](http://www.erlang.org/doc/)
* [What's OTP?](http://learnyousomeerlang.com/what-is-otp)
