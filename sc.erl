-module(sc).
-compile(export_all).

test() ->
    start(),
    init(),
    play(10).

play(0) ->
    true;
play(N) ->
    play_70(),
    play(N-1).

init() ->
    send(["/dumpOSC", 3]),
    send(["/clearSched"]), %%       - clear all scheduled bundles
    send(["/g_freeAll", 0]),
    send(["/notify", 1]),  %%    - send me notifications
    send(["/d_loadDir", "/Applications/Sonic Pi.app/etc/synthdefs/compiled"]),
    send(["/sync", 1]),
    send(["/b_allocRead", 0, "/Applications/Sonic Pi.app/etc/buffers/rand-stream.wav", 0, 0]),
    send(["/sync", 2]),
    send(["/b_query", 0]),
    send(["/clearSched"]),
    send(["/g_freeAll", 0]),
    send(["/g_new",2, 0, 0]), %%  add a new group ... fail duplicate node
    send(["/g_new",3, 2, 2]),
    send(["/g_new",4, 2, 3]),
    send(["/g_new",5, 3, 2]),
    send(["/s_new", "sonic-pi-mixer", 6, 0, 2, "in_bus", 10]),
    send(["/status"]),
    send(["/sync", 3]).

play_70() ->
    send(["/g_new", 7, 1, 4]),
    send(["/s_new", "sonic-pi-basic_mixer", 8, 0, 2, 
	  %% 8 = syn ID
	  %% 0 it's an add
	  %% add target = 2
	  amp, 1, 
	  amp_slide, 0.1, 
	  amp_slide_shape, 1, 
	  amp_slide_curve, 0, 
	  "in_bus", 12, 
	  "amp", 0.3,
	  "out_bus", 10]),
    send(["/s_new", "sonic-pi-beep", 9, 0, 7, note, 70.0, "out_bus", 12,
	  "/n_set", 8, "amp_slide", 1.0]),
    send(["/n_set", 8, "amp_slide", 1.0]),
    sleep(1000),
    send(["/n_set", 8, "amp", 0.0]),
    send(["/n_free", 8]),
    send(["/n_free", 7]).

start() ->
    S = self(),
    register(server, spawn(fun() -> go(S) end)),
    receive
	ack ->
	    true
    end.

send(M) ->
    server ! {send, M}.

sleep(T) ->
    receive
	after T ->
		true
	end.

go(P) ->
    {ok, Socket} = gen_udp:open(0,[binary]),
    P ! ack,
    io:format("Socket:~p~n",[Socket]),
    loop(Socket).

loop(Socket) ->
    receive
	{send, M} ->
	    E = encode(M),
	    io:format("Send: ~p~n",[M]),
	    gen_udp:send(Socket, "localhost", 4556, E),
	    loop(Socket);
	{udp, Socket, _Ip, 4556, Bin} ->
	    Msg = osc:decode(Bin),
	    io:format("received: ~p~n",[Msg]),
	    loop(Socket);
	Any ->
	    io:format("Any:~p~n",[Any]),
	    loop(Socket)
    end.


encode(M) ->
    osc:encode(M).
