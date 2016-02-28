-module(message_server).
-behaviour(gen_server).
-export([start/0, stop/1, new_message/2]).
-export([init/1, handle_cast/2, terminate/2]).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

new_message(Pid, Message) ->
    gen_server:cast(Pid, {new_message, Message}).
    

% Module functions implemented
init([]) ->
    {ok, []}.

handle_cast({new_message, Message}, MessageHistory) ->
    {noreply, [Message|MessageHistory]}.

terminate(_Reason, _MessageHistory) ->
    ok. % fix this
