-module(server).
-export([start/0]).
% make this a event_consumer/event_manager

-record(event_manager_state, {message_server,
			      name_server,
			      client_handlers = [] }).

%% callable functions
start() ->
    Host = "localhost",
    Port = 9998,
    {ok, MessageServer} = message_server:start(),
    {ok, NameServer} = name_server:start(),
    EventManagerInitialState = #event_manager_state{
			   message_server=MessageServer,
			   name_server=NameServer},
    EventManager = spawn(fun() -> loop(EventManagerInitialState) end),
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

handle_event({request_usernames, ClientHandler}, State) ->
    NameServer = State#event_manager_state.name_server,
    NameList = name_server:get_names(NameServer),
    client_handler:print_users(ClientHandler, NameList),
    {ok, State};

handle_event({new_message, Message}, State) ->
    MessageServer = State#event_manager_state.message_server,
    ClientHandlers = State#event_manager_state.client_handlers,
    message_server:new_message(MessageServer, Message),
    lists:foreach(fun(ClientHandle) -> client_handler:new_message_from_server(ClientHandle, Message) end, ClientHandlers),
    {ok, State};

handle_event({new_login, Username, ClientHandler}, State) ->
    NameServer = State#event_manager_state.name_server,
    MessageServer = State#event_manager_state.message_server,
    MessageHistory = message_server:get_messages(MessageServer),
    name_server:new_name(NameServer, ClientHandler, Username),
    client_handler:login(ClientHandler, Username), % only do this if login succseeds
    client_handler:print_history(ClientHandler, MessageHistory),
    {ok, State};

handle_event({new_client_handler, NewClientHandler}, State) ->
    OldClientHandlers = State#event_manager_state.client_handlers,
    client_handler:subscribe(NewClientHandler),
    NewState = State#event_manager_state{client_handlers = [NewClientHandler|OldClientHandlers]},
    {ok, NewState}.
