-module(midi_player).

-compile(export_all).

test() ->
    midi_event_gen:start(),
    play("jerusalem.mid"),
    %% elib2_misc:dump("jert.tmp", V).
    true.

play(F) ->
    L = midi_parser:parse_file(F),
    L1 = [normalise(I) || I <-L],
    L2 = lists:append(L1),
    L3 = lists:sort(L2),
    play1(0, L3).

play1(Time, [{Time,D}|T]) ->
    do(D),
    play1(Time, T);
play1(Time1, [{Time2,D}|T]) ->
    delay(Time2-Time1),
    do(D),
    play1(Time2, T).

delay(T) ->
    %% io:format("sleep:~p~n",[T]),
    timer:sleep(trunc(T*3.5)).

do({programChange, Channel, K}) ->
    %% Name = midi_sounds:sound_name(K),
    %% io:format("setting Channel:~p to ~p:~s~n",[Channel,K,Name]),
    midi_event(16#C0 + Channel, K, 0);

do({controllerChange, Channel, Pitch, Vol}) ->
    midi_event(16#B0 + Channel, Pitch, Vol);
do({noteOn, Channel, Pitch, Vol}) ->
    midi_event(16#90 + Channel, Pitch, Vol);
do({noteOff, Channel, Pitch, Vol}) ->
    midi_event(16#80 + Channel, Pitch, Vol);
do({lyric, X}) ->
    io:format("~s ~n",[X]);
do(X) ->
    io:format("dropping:~p~n",[X]).

normalise(#{type := track, data := D}) ->
    add_times(0, D, []);
normalise(_) -> [].

add_times(T1, [{T2,D}|T], L) ->
    T3 = T1 + T2,
    add_times(T3, T, [{T3,D}|L]);
add_times(_, [], L) ->
    lists:reverse(L).

midi_event(X,Y,Z) ->
    midi_event_gen:send([X,Y,Z]).
