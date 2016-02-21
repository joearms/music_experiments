-module(osc_scheduler).
-compile(export_all).

%% We run on port 8014
server_port() -> 8014.

start() ->
    S = self(),
    register(?MODULE, spawn(fun() -> go(S) end)),
    receive
	ack ->
	    true
    end.

go(P) ->
    {ok, Socket} = gen_udp:open(server_port(), [binary]),
    io:format("Osc scheduler listening on port:~p~n",[server_port()]),
    P ! ack,
    loop(Socket).

loop(Socket) ->
    receive
	{udp, Socket, _IP, _Port, Bin} ->
	    %% io:format("received:~p~n",[Bin]),
	    %% T2 = erlang:system_time()/1000000000,
	    %% io:format("received packet :~p~n",[T2]),
	    handle_message(Socket, Bin),
	    loop(Socket);
	Any ->
	    io:format("Any:~p~n",[Any]),
	    loop(Socket)
    end.

handle_message(Socket, Bin) ->
    %% io:format("received:~p~n",[Bin]),
    case osc:decode(Bin) of
	{bundle,Time,[{_Len,B1}]} ->
	    Cmd = osc:decode(B1),
	    %% io:format("Cmd=~p~n",[Cmd]),
	    do_command(Socket, Cmd, Time)

    end.

do_command(Socket, {cmd, ["/forward", Host, Port | Cmd]}, Time) ->
    %% Build the comand to send
    Bin = osc:encode(Cmd),
    at_time_send(Host, Port, Time, Socket, Bin);
do_command(_, X, _) ->
    io:format("invalid command:~p~n", [X]).


at_time_send(Host, Port, Time, Socket, Bin) ->
    %% io:format("at time ~p send",[Time]),
    spawn(fun() ->
		  send_message_at_time(Host, Port, Time, Socket, Bin)
	  end).

send_message_at_time(Host, Port, WantedTime, Socket, Bin) ->
    Tnow  = my_now(),
    Delta = trunc((WantedTime - Tnow)*1000),
    if 
	Delta > 0 ->
	    erlang:send_after(Delta, self(), ping),
	    receive
		ping ->
		    gen_udp:send(Socket, Host, Port, Bin)
	    end;
	true ->
	    void
    end.

my_now() ->
    %% seconds past epoc 
    erlang:system_time()/1000000000.


	     

    

  



    


	


