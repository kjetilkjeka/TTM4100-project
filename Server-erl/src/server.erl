-module(server).
-export([start/0]).
% make this a event_consumer/event_manager

%% callable functions
start() ->
    Host = "localhost",
    Port = 9998,
    {ok, MessageServer} = message_server:start(),
    EventManager = spawn(fun() -> loop([]) end),
    {ok, ThreadedTcpServer} = threaded_tcp_server:start(EventManager, Port, Host),
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
