-module(server).
-export([start/0]).
% make this a event_consumer/event_manager

%% callable functions
start() ->
    Host = "localhost",
    Port = 9998,
    {ok, MessageServer} = message_server:start(),
    EventManager = spawn(fun() -> loop([]) end),
    ThreadedTcpServer = spawn(fun() -> threaded_tcp_server_init(EventManager, Port, Host) end),
    {ok, EventManager}.

%% implemented events (since this is event manager)


loop(State) ->
    receive
	{new_event, Event} ->
	    {ok, NewState} = handle_event(Event, State)
    end,
    io:format(standard_io, "New State is ~w~n", [NewState]),
    loop(NewState).

handle_event({new_message, Message}, State) ->
    %message_server:new_message(MessageServer, Message),
    lists:foreach(fun(ClientHandle) -> client_handler:new_message_from_server(ClientHandle, Message) end, State),
    {ok, State};
handle_event({new_client_handler, ClientHandle}, State) ->
    NewState = [ClientHandle|State],
    client_handler:subscribe(ClientHandle),
    {ok, NewState}.



	    

% We need soemthing waiting on the servers
threaded_tcp_server_init(EventManager, Port, Host) ->
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

