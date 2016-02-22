-module(midi_event_listener).

-compile(export_all).

start() ->
    register(?MODULE, 
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Port = open_port({spawn, "./midi_event_listener"}, 
					    [{packet, 2}]),
			   io:format("Port=~p~n",[Port]),
			   loop(Port)
		   end)).

sleep(T) ->
    receive
	after T ->
		true
	end.

stop() ->
    ?MODULE ! stop.

send(M) -> call_port([1|M]).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{?MODULE, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {?MODULE, Data}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason});
	{Port,{data,Data}} ->
	    io:format("~n~p bytes~n",[length(Data)]),
	    Data1 = cvt(list_to_binary(Data)),
	    io:format("event:~p~n",[Data1]),
	    loop(Port);
	Any ->
	    io:format("Received:~p~n",[Any]),
	    loop(Port)
    end.
	
cvt(<<1,T:64/unsigned-little-integer,B/binary>>) ->
    {T, B}.

