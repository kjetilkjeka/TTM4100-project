-module(chat_server).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:load(jsx),
    application:start(jsx),
    application:start(chat_server),
    ok.
    

start(_StartType, _StartArgs) ->
    {ok, Pid} = server:start(),
    {ok, Pid}.
    %chat-server_sup:start_link().

stop(_State) ->
    ok.
