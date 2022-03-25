# Erlang-communication-app

Before starting, this application is written on *Erlang/OTP 22*.
____________
Each server, client can run on each different node in a cluster.

NOTE: *Nodes communicate in a cluster, must have the same secret cookie*. Get cookie:
```
erlang:get_cookie().
```
- chat_server
  - Create a node and set a secret cookie to launch the server
  ```
  erl -name chat_server@ipaddress -setcookie 'secret'
  ```
  - Compile *chat_server.erl* then start global gen_server:
  ```
  c(chat_server).
  chat_server:start_link().
  ```
- chat_client1

  - Open other Shell and run
  ```
  erl -name chat_client1@ipaddress -setcookie 'secret'
  ```
  - Compile *chat_client1.erl* then start local gen_server

  ```
  c(chat_client1).
  chat_client1:start_link().
  ```
  - Connect to server and remember the name on it
  ```
  chat_client1:connect_server('chat_server@ipaddress').
  chat_client1:chat_register().
  ```

- chat_client2
  ```
  erl -name chat_client2@ipaddress -setcookie 'secret'
  ```
  ```
  c(chat_client2).
  chat_client2:start_link().
  chat_client2:connect_server('chat_server@ipaddress').
  chat_client2:chat_register().
  ```
  - Now we can chat with chat_client1
  ```
  chat_client2:send_msg('chat_client1@ipaddress', chat_client1, "Hello").
  ```
