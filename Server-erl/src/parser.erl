-module(parser).
-export([encode_data/4, decode_data/1]).

decode_data(BinaryData) ->
    DataJson = jsx:decode(BinaryData, [{labels, atom}, return_maps]),
    Request = binary_to_atom(maps:get(request, DataJson), utf8), % fix, make safe
    Content = binary_to_list(maps:get(content, DataJson)),
    {ok, {Request, Content}}.

encode_data(history, Timestamp, Sender, History) ->
    ListOfLists = History,
    ListOfBinaries = lists:map(fun(String) -> jsx:decode(String) end, ListOfLists),
    Content = ListOfBinaries,
    Message = jsx:encode([
			  {<<"timestamp">>, <<Timestamp>>},
			  {<<"sender">>,list_to_binary(Sender)},
			  {<<"response">>,<<"history">>},
			  {<<"content">>,ListOfBinaries}
			 ]);
encode_data(Response, Timestamp, Sender, Content) ->
    Message = jsx:encode([
			  {<<"timestamp">>, <<Timestamp>>},
			  {<<"sender">>,list_to_binary(Sender)},
			  {<<"response">>,atom_to_binary(Response, utf8)},
			  {<<"content">>,list_to_binary(Content)}
			 ]).

    
