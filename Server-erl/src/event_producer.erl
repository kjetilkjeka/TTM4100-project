-module(event_producer).
-export([event/2]).

-callback subscribe(Pid :: pid()) -> 'ok'.

event(Subscribers, Event) ->
    lists:foreach(fun(Subscriber) -> Subscriber ! {new_event, Event} end, Subscribers).
			   
			  
