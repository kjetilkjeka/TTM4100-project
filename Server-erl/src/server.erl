-module(server).
-export([start/0]).

start() ->
    ok.

% We need a client handler for keeping communication with clients
client_handler() ->
    ok.

% We need soemthing waiting on the servers 
threaded_tcp_server() ->
    ok.

% We also need something for managing communication between client handlers
client_coordinator() ->
    ok.
