-module(parser).
-export([encode_data/3, decode_data/1]).

decode_data(BinaryData) ->
    DataJson = jsx:decode(BinaryData, [{labels, atom}, return_maps]),
    Request = binary_to_atom(maps:get(request, DataJson), utf8), % fix, make safe
    ContentRaw = maps:get(content, DataJson),
    Content = case ContentRaw of
		  null ->
		      "null";
		  _ ->
		      binary_to_list(ContentRaw)
	      end,
    %Content = binary_to_list(maps:get(content, DataJson)),
    {ok, {Request, Content}}.

make_timestamp() ->
    {Date, Time} = calendar:local_time(),
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    TimeStampString = 
	integer_to_list(Day) ++ "." ++ integer_to_list(Month) ++ "." ++ integer_to_list(Year) ++ ", "
	++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second),
    _TimeStamp = list_to_binary(TimeStampString).
							

encode_data(history, Sender, History) ->
    ListOfLists = History,
    %ListOfBinaries = lists:map(fun(String) -> jsx:decode(String) end, ListOfLists),
    %Content = ListOfBinaries,
    Message = jsx:encode([
			  {<<"timestamp">>, make_timestamp()},
			  {<<"sender">>,list_to_binary(Sender)},
			  {<<"response">>,<<"history">>},
			  {<<"content">>,History}
			 ]);
encode_data(Response, Sender, Content) ->
    Message = jsx:encode([
			  {<<"timestamp">>, make_timestamp()},
			  {<<"sender">>,list_to_binary(Sender)},
			  {<<"response">>,atom_to_binary(Response, utf8)},
			  {<<"content">>,list_to_binary(Content)}
			 ]).

    
