-module(client_handler).
-export([start/1]).

start(Socket) ->
    Pid = spawn(fun() -> loop(Socket) end),
    gen_tcp:controlling_process(Socket, Pid), % make sure he can take control of socket
    {ok, Pid}.
			

loop(State) ->
    receive
	{tcp, S, Data} ->
	    {ok, NewState} = handle_tcp(S, Data, State)
    end,
    loop(NewState).

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
	    %message_server:new_message(MessageServer, Message), % should not realy be content
	    gen_tcp:send(Socket, Message), % just sending back to receiving socket
	    ok; %temp
	"names" ->
	    ok;
	"help" ->
	    ok
    end,
    {ok, State}.
