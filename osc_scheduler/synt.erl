-module(synt).
-compile(export_all).

start() ->
    Port = 6000,
    io:format("Synt waiting on port 6000~n"),
    {ok, Socket} = gen_udp:open(Port, [binary]),
    loop(Socket).

loop(Socket) ->
    receive
	{udp, Socket, _IP, _Port, Bin} ->
	    T1 = osc:now(),
	    {cmd,D} = osc:decode(Bin),
	    io:format("Synt (time ~p) :: ~p~n",[T1, D]),
	    loop(Socket);
	Other ->
	    io:format("Synt received ~p~n",[Other]),
	    loop(Socket)
    end.
