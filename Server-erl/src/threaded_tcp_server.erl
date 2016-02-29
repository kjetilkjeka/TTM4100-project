-module(threaded_tcp_server). % realy not threaded
-export([start/3]).


start(EventManager, Port, Host) ->
    Pid = spawn(fun() -> init(EventManager, Port, Host) end),
    {ok, Pid}.

	   
% We need soemthing waiting on the servers
init(EventManager, Port, Host) ->
    %io:format("Server started~n"),
    {ok, IpAddress} = inet:getaddr(Host, inet),
    %io:format("ip adress resolved, it is: ~w~n", [IpAddress]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {ip, IpAddress}, {packet, 0}, {active, true}, inet]),
    threaded_tcp_server(EventManager, ListenSocket).

threaded_tcp_server(EventManager, ListenSocket) -> % name is bad to be consistent with python server
    %io:format("Init thing done~n"),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    %io:format("Socket thing done~n"),
    io:format(standard_io, "new client connected ~n", []),
    {ok, ClientHandler} = client_handler:start(Socket),
    event_producer:event([EventManager], {new_client_handler, ClientHandler}), % fix hack
    threaded_tcp_server(EventManager, ListenSocket).

