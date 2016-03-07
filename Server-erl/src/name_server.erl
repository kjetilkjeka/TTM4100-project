-module(name_server).
-behaviour(gen_server).
-export([start/0, stop/1, new_name/3, get_names/1, remove_client/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

new_name(Pid, ClientHandler, Name) ->
    gen_server:call(Pid, {new_name, ClientHandler, Name}).

get_names(Pid) ->
    gen_server:call(Pid, get_names).

remove_client(Pid, ClientHandler) ->
    gen_server:cast(Pid, {remove_client, ClientHandler}).

% Module callback functions for gen_server

init([]) ->
    {ok, {maps:new(), maps:new()}}. % make some record of this

handle_cast({remove_client, ClientHandler}, State) ->
    {HandlerMap, NameMap} = State,
    Name = maps:get(ClientHandler, HandlerMap),
    NewNameMap = maps:remove(Name, NameMap),
    NewHandlerMap = maps:remove(ClientHandler, HandlerMap),
    NewState = {NewHandlerMap, NewNameMap},
    {noreply, NewState}.

handle_call(get_names, _From, State) ->
    {_HandlerMap, NameMap} = State,
    UserList = maps:keys(NameMap),
    {reply, UserList, State};
handle_call({new_name, ClientHandler, Name}, _From, State) ->
    {HandlerMap, NameMap} = State,
    UsernameExists = maps:is_key(Name, NameMap),
    case UsernameExists of
	true ->
	    NewState = State,
	    Reply = {error, username_exists};
	false ->
	    NewNameMap = maps:put(Name, ClientHandler, NameMap),
	    NewHandlerMap = maps:put(ClientHandler, Name, HandlerMap),
	    NewState = {NewHandlerMap, NewNameMap},
	    Reply = ok
    end,
    {reply, Reply, NewState}.

terminate(_Reason, _MessageHistory) ->
    ok. % fix this
    
	    
    
       
    
