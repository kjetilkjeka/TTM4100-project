-module(client_handler).
-export([start/1, subscribe/1, new_message_from_server/2]).
-behaviour(event_producer).
-record(client_handler_state, {socket, event_subscribers=[] }).
			       

%% callable function (interface)
start(Socket) ->
    Pid = spawn(fun() -> loop(#client_handler_state{socket=Socket}) end),
    gen_tcp:controlling_process(Socket, Pid), % make sure he can take control of socket
    {ok, Pid}.

subscribe(Pid) ->
    Pid ! {subscribe, self()}.

new_message_from_server(Pid, Message) -> % fix name
    Pid ! {new_message, Message}.

%% required implemented as callbacks in subscribed module (callback interface)
event_new_message(State, Message) -> % fix name
    Subscribers = State#client_handler_state.event_subscribers,
    event_producer:event(Subscribers, {new_message, Message}).

			

%% Process functions
loop(State) ->
    receive
	{tcp, S, Data} ->
	    {ok, NewState} = handle_tcp(S, Data, State);
	{new_message, Message} ->
	    {ok, NewState} = handle_message(Message, State);
	{subscribe, NewSubscriber} ->
	    OldSubscribers = State#client_handler_state.event_subscribers,
	    {ok, NewState} = {ok, State#client_handler_state{event_subscribers=[NewSubscriber|OldSubscribers]}}
    end,
    loop(NewState).

handle_message(Message, State) ->
    Socket = State#client_handler_state.socket,
    io:format(standard_io, "Sending back message~n", []),
    gen_tcp:send(Socket, Message),
    {ok, State}.
   

handle_tcp(Socket, DataBinary, State) ->
    DataJson = jsx:decode(DataBinary, [{labels, atom}, return_maps]),
    Request = binary_to_list(maps:get(request, DataJson)),
    Content = binary_to_list(maps:get(content, DataJson)),
    io:format(standard_io, "Received packet ~w, with Request: ~s, and content: ~s~n", [DataJson, Request, Content]),
    case Request of % mind that only lowercase is allowed
	"login" ->
	    ok; %temp
	"logout" ->
	    ok; %temp
	"msg" ->
	    Message = jsx:encode([
				  {<<"timestamp">>, <<1>>},
				  {<<"sender">>,<<"Username">>},
				  {<<"response">>,<<"Message">>},
				  {<<"content">>,list_to_binary(Content)}
				 ]),
	    event_new_message(State, Message),
	    ok; %temp
	"names" ->
	    ok;
	"help" ->
	    ok
    end,
    {ok, State}.