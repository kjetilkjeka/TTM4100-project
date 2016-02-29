-module(server).
-export([start/0]).

start() ->
    Host = "localhost",
    Port = 9998,
    {ok, MessageServer} = message_server:start(),
    ThreadedTcpServer = spawn(fun() -> threaded_tcp_server_init(Port, Host) end).


% We need soemthing waiting on the servers
threaded_tcp_server_init(Port, Host) ->
    %io:format("Server started~n"),
    {ok, IpAddress} = inet:getaddr(Host, inet),
    %io:format("ip adress resolved, it is: ~w~n", [IpAddress]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {ip, IpAddress}, {packet, 0}, {active, true}, inet]),
    threaded_tcp_server(ListenSocket).

threaded_tcp_server(ListenSocket) -> % name is bad to be consistent with python server
    %io:format("Init thing done~n"),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    %io:format("Socket thing done~n"),
    io:format(standard_io, "new client connected ~n", []),
    {ok, _ClientHandler} = client_handler:start(Socket),
    threaded_tcp_server(ListenSocket).

