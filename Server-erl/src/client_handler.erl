-module(client_handler).
-export([start/1, subscribe/1, new_message_from_server/2, print_history/2]).
-behaviour(event_producer).
-record(client_handler_state, {socket, event_subscribers=[] }).
			       

%% callable function (interface)
start(Socket) ->
    Pid = spawn(fun() -> loop(#client_handler_state{socket=Socket}) end),
    gen_tcp:controlling_process(Socket, Pid), % make sure he can take control of socket
    {ok, Pid}.

subscribe(Pid) ->
    Pid ! {subscribe, self()}.

new_message_from_server(Pid, Message) -> % fix name (call it print message)
    Pid ! {new_message, Message}.

print_history(Pid, History) ->
    Pid ! {print_history, History}.

%% required implemented as callbacks in subscribed module (callback interface)
event_new_message(State, Message) -> % fix name
    Subscribers = State#client_handler_state.event_subscribers,
    event_producer:event(Subscribers, {new_message, Message}).

event_new_login(State, Username) ->
    Subscribers = State#client_handler_state.event_subscribers,
    event_producer:event(Subscribers, {new_login, Username, self()}).
    
			

%% Process functions
loop(State) ->
    receive
	{tcp, S, Data} ->
	    {ok, NewState} = handle_tcp(S, Data, State);
	{new_message, Message} ->
	    {ok, NewState} = handle_message(Message, State);
	{subscribe, NewSubscriber} ->
	    OldSubscribers = State#client_handler_state.event_subscribers,
	    {ok, NewState} = {ok, State#client_handler_state{event_subscribers=[NewSubscriber|OldSubscribers]}};
	{print_history, History} ->
	    {ok, NewState} = handle_history(History, State)
    end,
    loop(NewState).

handle_history(History, State) ->
    Socket = State#client_handler_state.socket,
    Message = parser:encode_data(history, 1, "SomeSender", History),
    gen_tcp:send(Socket, Message),
    {ok, State}.
    

handle_message(Message, State) ->
    Socket = State#client_handler_state.socket,
    io:format(standard_io, "Sending back message~n", []),
    gen_tcp:send(Socket, Message),
    {ok, State}.
   

handle_tcp(Socket, BinaryData, State) ->
    {ok, {Request, Content}} = parser:decode_data(BinaryData),
    io:format(standard_io, "Received packet ~w, with Request: ~s, and content: ~s~n", [BinaryData, Request, Content]),
    case Request of % mind that only lowercase is allowed
	login ->
	    Username = Content,
	    event_new_login(State, Username),
	    ok; %temp
	logout ->
	    ok; %temp
	msg ->
	    Message = parser:encode_data(message, 1, "Username", Content),
	    event_new_message(State, Message),
	    ok; %temp
	names ->
	    ok;
	help ->
	    ok
    end,
    {ok, State}.
