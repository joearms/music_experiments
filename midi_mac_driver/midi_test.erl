-module(midi_test).

-compile(export_all).

test() ->
    midi_event_gen:start(),
    send_notes(),
    midi_event_gen:stop(),
    init:stop().

send_notes() ->
    L = notes(),
    send_notes(L).

send_notes([{sleep,I}|T]) -> 
    sleep(I),
    send_notes(T);
send_notes([B|T]) ->
    io:format("sending~p~n",[B]),
    midi_event_gen:send(B),
    send_notes(T);
send_notes([]) ->
    true.

sleep(T) ->
    receive
	after T
		  ->
		true
	end.

notes() ->
    lists:flatten([<<192,2,0>> | scale(60,69)]).

scale(J,K) ->
    [[<<144,I,50>>,{sleep,150},<<144,I,0>>] || I <- lists:seq(J,K)].
