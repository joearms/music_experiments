-module(sonic_pi_emulator).

-compile(export_all).

start() ->
    %% start a synt
    spawn(fun() -> synt:start() end),
    %% spawn a relay
    osc_scheduler:start(),
    %% open a socket
    {ok, Socket} = gen_udp:open(0, [binary]),
    %% read the clock
    T = osc:now(),
    %% scedule some future events
    send(Socket, T+2,   ["/forward", "localhost", 6000, "/sendmidi", 12, 34, 56]),
    send(Socket, T+2.3,   ["/forward", "localhost", 6000, "/dothis", 1, 22, 3.134]),
    send(Socket, T+1, ["/forward", "localhost", 6000, "/andthat", 6.7, "hello", 123]),
    %% wait forever (because we're launched from a makefile)
    receive
	after infinity ->
		true
	end.


send(Socket, Time, Data) ->
    io:format("At ~p do:~p~n",[Time,Data]),
    Bin = osc:pack_ts(Time, Data),
    gen_udp:send(Socket, "localhost", 8014, Bin).

	
