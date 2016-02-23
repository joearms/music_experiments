-module(midi_parser).
-compile(export_all).

info() -> info("rach-pc1-1.mid").

test() ->
    E = parse_file("jerusalem.mid"),
    E.

parse_file(F) ->
    {ok, Bin} = file:read_file(F),
    parse_bin(Bin).

parse_bin(B) ->
    Chunks = collect_chunks(B, []),
    [parse_chunk(I) || I <- Chunks].

%%----------------------------------------------------------------------

collect_chunks(<<"MThd", Len:32, B:Len/binary,Rest/binary>>, L) ->
    collect_chunks(Rest, [{header,B}|L]);
collect_chunks(<<"MTrk", Len:32, B:Len/binary,Rest/binary>>, L) ->
    collect_chunks(Rest, [{track,B}|L]);
collect_chunks(<<>>, L) ->
    lists:reverse(L).

%%----------------------------------------------------------------------

parse_chunk({header,<<Format:16,Tracks:16,TimeDiv:2/binary>>}) ->
    #{type => header, format=>Format, tracks => Tracks, time => get_ticks(TimeDiv)};
parse_chunk({track, Bin}) ->
    #{type => track, data => parse_track(Bin, <<>>, [])}.

parse_track(<<>>, _Status, Acc) ->
    lists:reverse(Acc);
parse_track(DataWTime, Status, Acc) ->
    {Delta, Data} = get_varint(DataWTime, 0),
    case get_next_event(Data, Status) of
	{NewStatus, {Event, Rest}} ->
	    parse_track(Rest, NewStatus, [{Delta, Event} | Acc]);
	{Event, Rest} ->
	    parse_track(Rest, Status, [{Delta, Event} | Acc])
    end.

get_next_event(<<Status, _/binary>> = X, _) when Status >= 16#80 ->
    {Status, get_event(X)};
get_next_event(Data, RunningStatus) ->
    {RunningStatus, get_event(<<RunningStatus, Data/binary>>)}.

get_event(<<16#FF,3,Len:8,Str:Len/binary, Rest/binary>>) ->
    {{trackName,Str}, Rest};
get_event(<<16#FF,4,Len:8,Str:Len/binary, Rest/binary>>) ->
    {{instrumentName,Str}, Rest};
get_event(<<16#FF,5,Len:8,Str:Len/binary, Rest/binary>>) ->
    {{lyric,Str}, Rest};
get_event(<<16#FF,16#2f,0,Rest/binary>>) ->
    {endOfTrack, Rest};
get_event(<<16#FF,16#51,3,T:24,Rest/binary>>) ->
    {{setTempo,T}, Rest};
get_event(<<16#FF,16#58,4,NN,DD,CC,BB,Rest/binary>>) ->
    {{timeSig, NN,DD, CC, BB}, Rest};
get_event(<<16#FF,16#59,2,SF,MI,Rest/binary>>) ->
    {{keySig, SF,MI}, Rest};
get_event(<<16#FF,16#7F,Len,B:Len/binary,Rest/binary>>) ->
    io:format("???~p~n",[B]),
    {{extension,B}, Rest};
get_event(<<8:4,C:4, K, W, R/binary>>) ->
    {{noteOff, C, K, W}, R};
get_event(<<9:4,C:4, K, W, R/binary>>) ->
    {{noteOn, C, K, W}, R};
get_event(<<16#A:4,C:4, K, W, R/binary>>) ->
    {{polyPhonicKeyPressure, C, K, W}, R};
get_event(<<16#B:4,C:4, K, W, R/binary>>) ->
    {{controllerChange, C, K, W}, R};
get_event(<<16#C:4,C:4, K, R/binary>>) ->
    {{programChange, C, K}, R};
get_event(<<16#D:4,C:4, K, R/binary>>) ->
    {{channelPressure, C, K}, R};
get_event(<<16#E:4,C:4, K1, K2, R/binary>>) ->
    {{pitchBend, C, K1, K2}, R}.


%%----------------------------------------------------------------------

get_varint(<<0:1, N:7/integer-unsigned, B/binary>>, Acc) ->
    {Acc bsl 7 + N, B};
get_varint(<<1:1, N:7/integer-unsigned, B/binary>>, Acc) ->
    get_varint(B, Acc bsl 7 + N).


%%----------------------------------------------------------------------
%% rewrite ...

get_ticks(<<1:1, SMPTEFrames:7/integer-unsigned, TicksPerFrame:16/integer-unsigned>>) ->
    Milliseconds = case SMPTEFrames of
		       128-29 ->
			   29.97 * TicksPerFrame;
		       _ ->
			   (128-SMPTEFrames) * TicksPerFrame
		   end,
    {milliseconds, Milliseconds};
get_ticks(<<0:1, TicksPerQuarter:15/integer-unsigned>>) ->
    {ticks_per_quarter_note, TicksPerQuarter}.


info(F) ->
    P = parse_file(F),
    O = [summary(I) || I <- P],
    O.


summary(#{format := _} = X) ->
    {header, X};
summary(#{data:=D} ) ->
    {track, get_elements(D, #{})}.

get_elements([], D) ->
    D;
get_elements([{_,E}|T], D) ->
    Tag = type(E),
    case maps:find(Tag, D) of
	error -> get_elements(T, maps:put(Tag, 1, D));
	{ok, N}->get_elements(T, maps:put(Tag, N+1, D))
    end.
	    
type(X) when is_tuple(X) ->
    element(1, X);
type(X) ->
    X.






    
