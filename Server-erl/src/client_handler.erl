-module(client_handler).
-export([start/1, subscribe/1, new_message_from_server/2, print_history/2, login/2, print_users/2]).
-behaviour(event_producer).
-record(client_handler_state, {socket,
			       event_subscribers=[],
			       logged_in = false,
			       username = []
			      }).
		       
-define(ALLOWED_COMMANDS, [login, logout, msg, names, help]).


%% callable function (interface)
start(Socket) ->
    Pid = spawn(fun() -> loop(#client_handler_state{socket=Socket}) end),
    gen_tcp:controlling_process(Socket, Pid), % make sure he can take control of socket
    {ok, Pid}.

subscribe(Pid) ->
    Pid ! {subscribe, self()}.

new_message_from_server(Pid, Message) -> % fix name (call it print message)
    Pid ! {new_message, Message}.

print_history(Pid, History) -> % change name to login
    Pid ! {print_history, History}.

login(Pid, Username) ->
    Pid ! {login, Username}.

print_users(Pid, UserList) ->
    Pid ! {print_users, UserList}.
				  
    

%% required implemented as callbacks in subscribed module (callback interface)
event_new_message(State, Message) -> % fix name
    Subscribers = State#client_handler_state.event_subscribers,
    event_producer:event(Subscribers, {new_message, Message}).

event_new_login(State, Username) ->
    Subscribers = State#client_handler_state.event_subscribers,
    event_producer:event(Subscribers, {new_login, Username, self()}).

event_request_usernames(State) ->    
    Subscribers = State#client_handler_state.event_subscribers,
    event_producer:event(Subscribers, {request_usernames, self()}).
			

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
	    {ok, NewState} = handle_history(History, State);
	{login, Username} ->
	    {ok, NewState} = handle_login(Username, State);
	{print_users, UserList} ->
	    {ok, NewState} = handle_userlist(UserList, State)
    end,
    loop(NewState).

handle_userlist(UserList, State) ->
    Socket = State#client_handler_state.socket,
    StartText = "Online users are:",
    UserListContent = lists:foldl(fun(User, AccString) ->
					  AccString ++ "\n" ++ User end,
				  StartText,
				  UserList),
    Message = parser:encode_data(info, "Server", UserListContent),
    gen_tcp:send(Socket, Message),
    {ok, State}.
	    

handle_login(Username, State) ->
    NewState = State#client_handler_state{username = Username,
					  logged_in = true
					 },
    {ok, NewState}.

handle_history(History, State) ->
    Socket = State#client_handler_state.socket,
    Message = parser:encode_data(history, "Server", History),
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
	    Message = parser:encode_data(info, "Server", "Succesfully logged out"),
	    gen_tcp:send(State#client_handler_state.socket, Message),
	    terminate(State); %temp
	msg when State#client_handler_state.logged_in ->
	    Username = State#client_handler_state.username,
	    Message = parser:encode_data(message, Username, Content),
	    event_new_message(State, Message),
	    ok; %temp
	msg when not State#client_handler_state.logged_in ->
	    Message = parser:encode_data(error, "Server", "Not logged in"),
	    gen_tcp:send(State#client_handler_state.socket, Message);
	names ->
	    event_request_usernames(State);
	help ->
	    StartText = "Available user commands is",
	    HelpText = lists:foldl(fun(Command, AccString) ->
					   AccString ++ "\n" ++ command_info(Command) end,
				   StartText,
				   ?ALLOWED_COMMANDS),
	    Message = parser:encode_data(info, "Server", HelpText),
	    gen_tcp:send(State#client_handler_state.socket, Message)
    end,
    {ok, State}.

terminate(State) ->
    Socket = State#client_handler_state.socket,
    gen_tcp:close(Socket),
    exit(normal).
    




%% all the command stuff

command_info(login) ->
    "login <username> sends a request to login to the server. The content is a string with the username.";
command_info(logout) ->
    "logout sends a request to log out and disconnect from the server. The content is None.";
command_info(msg) ->
    "msg <message> sends a message to the server that should be broadcasted to all connected clients. The content is a string with the message.";
command_info(names) ->
    "names should send a request to list all the usernames currently connected to the server.";
command_info(help) ->
    "help sends a request to the server to receive a help text containing all requests supported by the server.";
command_info(Command) when is_atom(Command) ->
    atom_to_list(Command) ++ " no info given".


