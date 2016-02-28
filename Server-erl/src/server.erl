-module(server).
-export([start/0]).

start() ->
    Host = "localhost",
    Port = 9998,
    spawn(fun() -> threaded_tcp_server_init(Port, Host) end),
    ok.

% We need a client handler for keeping communication with clients
client_handler() ->
    ok.

% We need soemthing waiting on the servers
threaded_tcp_server_init(Port, Host) ->
    io:format("Server started~n"),
    {ok, IpAddress} = inet:getaddr(Host, inet),
    io:format("ip adress resolved, it is: ~w~n", [IpAddress]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {ip, IpAddress}, {packet, 0}, {active, false}, inet]),
    threaded_tcp_server(ListenSocket).

threaded_tcp_server(ListenSocket) -> % name is bad to be consistent with python server
    io:format("Init thing done~n"),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket thing done~n"),
    spawn(fun() -> client_coordinator(Socket) end), % maybe this onliner should be improved
    threaded_tcp_server(ListenSocket).

% We also need something for managing communication between client handlers
client_coordinator(Socket) ->
    io:format("new client connected ~n"),
    ok.
