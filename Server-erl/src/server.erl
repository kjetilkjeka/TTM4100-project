-module(server).
-export([start/0]).
% make this a event_consumer/event_manager

-record(event_manager_state, {message_server,
			       client_handlers = [] }).

%% callable functions
start() ->
    Host = "localhost",
    Port = 9998,
    {ok, MessageServer} = message_server:start(),
    EventManager = spawn(fun() -> loop(#event_manager_state{message_server=MessageServer}) end),
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
    MessageServer = State#event_manager_state.message_server,
    ClientHandlers = State#event_manager_state.client_handlers,
    message_server:new_message(MessageServer, Message),
    lists:foreach(fun(ClientHandle) -> client_handler:new_message_from_server(ClientHandle, Message) end, ClientHandlers),
    {ok, State};

handle_event({new_login, Username, ClientHandler}, State) ->
    MessageServer = State#event_manager_state.message_server,
    MessageHistory = message_server:get_messages(MessageServer),
    client_handler:print_history(ClientHandler, MessageHistory),
    {ok, State};

handle_event({new_client_handler, NewClientHandler}, State) ->
    OldClientHandlers = State#event_manager_state.client_handlers,
    client_handler:subscribe(NewClientHandler),
    NewState = State#event_manager_state{client_handlers = [NewClientHandler|OldClientHandlers]},
    {ok, NewState}.
