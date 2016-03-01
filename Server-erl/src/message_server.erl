-module(message_server).
-behaviour(gen_server).
-export([start/0, stop/1, new_message/2, get_messages/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

new_message(Pid, Message) ->
    gen_server:cast(Pid, {new_message, Message}).

get_messages(Pid) ->    
    gen_server:call(Pid, get_messages).

% Module functions implemented
init([]) ->
    {ok, []}.

handle_cast({new_message, Message}, MessageHistory) ->
    {noreply, MessageHistory ++ [Message]}.

handle_call(get_messages, _From, MessageHistory) ->
    {reply, MessageHistory, MessageHistory}.

terminate(_Reason, _MessageHistory) ->
    ok. % fix this
