-module(rpm).

%% reverse polish music language

%% To do
%%  add blank as separator
%%  a' moves up an octave
%%  a` moves down an octave
%%  a`- and a-` are obvious the same note
%%  display music in gtraphic notation
%%  make editr in browser

-compile(export_all).
-import(lists, [reverse/1]).

-define(TEMPO, 40).

start() ->
    midi_event_gen:start(internal),
    midi_test:send_notes().

test() ->
    parse_all_frags(),
    play_frag("start").


parse_all_frags()->
    [preparse_frag(I) || I <- frags()].

frags() ->
    ["f1:abc",
     "c3:{cge}",
     "t1:[c3]",
     "t2:([c3]s)2*",
     "f2:defeghi",
     "f4:d+eh+",
     "f5:ab>adac>ef",
     "b1t:(g>b-de-)2*",
     "b2t:g>b-de-fe-db-",
     "b1b:(<{gb-}d)4*",
     "b2b:({<g->b-}d)4*",
     "b1:{[b1t][b1b]}",
     "start:{([b1t][b2t])([b1b][b2b])}"
    ].


%% abc notes
%% (abc)         sequential
%% {abc}         play ABc in parallel
%% [xyz]         call subroutine abc
%% <TOS><int:N>* Do <TOS> N times
%% 8T            (set note length)
%% >             (increase by one octave)
%% <             (decrease by one octave)

%% <int>V        (set volume)
%% 
%%  .... aaa ... ( ... ) ... b ....
%%  Volumes and durations are *local* to the nearest containing (...)
%%  They are NOT applied within {...}

%%  {ceg} = chord
%%  (128V {seg}) loud chord

%% DV stack 


play1() ->
    midi_test:send_notes().
    
%% the ten minute langauge :-)
%% designed an implement in ten minutes

-define(IS_DIGIT(X),$0=<X,X=<$9).


play_frag(Name) ->
    case get({tune,Name}) of
	undefined ->
	    oops;
	Str ->
	    play(Str)
    end.

play(Str) ->
    play(Str, []).

play(Str, E) ->
    Parse = ex(Str),
    io:format("Parse=~p~n",[Parse]),
    {_,L1} = make_note_list(Parse, 0, []),
    io:format("notelist1:~p~n",[L1]),
    L2 = lists:sort(lists:reverse(L1)),
    io:format("notelist2:~p~n",[L2]),
    file:write_file("notelist2.tmp",[term_to_binary(L2)]), 
    play_live(L2).

play_live([{T1,X}|L1]) ->
    event(X),
    play_live(L1, T1).

play_live([{Time,X}|T], Time) ->
    event(X),
    play_live(T, Time);
play_live([{T2,X}|L], T1) ->
    Delay = (T2-T1)*?TEMPO,
    timer:sleep(Delay),
    event(X),
    play_live(L, T2);
play_live([], _) ->
    [].

event({noteOn,M,_,V}) ->
    %% io:format("noteOn:~p~n",[M]),
    midi_event_gen:send([144,M,V]);
event({noteOff, M}) ->
    midi_event_gen:send([144,M,0]).


par_play([H|T], T0, Ts, L) ->
    {Tstop, L1} = make_note_list([H], T0, L),
    par_play(T, T0, [Tstop|Ts], L1);
par_play([], _, Ts, L) ->
    {lists:max(Ts), L}.
	
make_note_list([$s|T], Time, L) ->
    %% silence -- just step the clock
    make_note_list(T, Time + 32, L);
make_note_list([{note,Note,Dur,Vol}|T], Time, L) ->
    Toff = trunc(Dur * 0.8),
    make_note_list(T, Time + Dur, [{Time, {noteOn, Note, Dur, Vol}},
				  {Time+Toff, {noteOff, Note}}|L]);
make_note_list([{call,S}|T2], Time0, L) ->
    case get({tune,S}) of
	undefined ->
	    oops;
	Str ->
	    Parse = ex(Str),
	    io:format("Parse=~p~n",[Parse]),
	    {Time1,L1} = make_note_list(Parse, Time0, L),
	    make_note_list(T2, Time1, L1)
    end;
make_note_list([{par,S}|T2], T0, L) ->
    %% play in parallel - take max of times
    {T1, L1} = par_play(S, T0, [], L),
    make_note_list(T2, T1, L1);
make_note_list([{seq,S}|T2], T0, L) ->
    make_note_list(S ++ T2, T0, L);
make_note_list([{loop,0,_}|T], T0, L) ->
    make_note_list(T, T0, L);
make_note_list([{loop,N,H}|T], T0, L) ->
    %% very nice H was TOS
    make_note_list([H,{loop,N-1,H}|T], T0, L);
make_note_list([{sharp,X}|T], T0, L) ->
    make_note_list([sharp(X)|T], T0, L);
make_note_list([], T, L) ->
    {T, L}.

sharp(N) when is_integer(N) ->
    1+N.


asci_to_midi($c) -> 60;
asci_to_midi($d) -> 62; 
asci_to_midi($e) -> 64; 
asci_to_midi($f) -> 65; 
asci_to_midi($g) -> 67; 
asci_to_midi($a) -> 57; 
asci_to_midi($b) -> 59.
 

preparse_frag(Str) ->
    {Name, Data} = preparse_frag(Str, []),
    put({tune,Name}, Data).

preparse_frag([$:|T], L) -> {reverse(L),T};
preparse_frag([H|T], L)  -> preparse_frag(T, [H|L]).

tests() ->
    t(0, "abc",      "abc"),
    t(1, "(a)",      [{seq, "a"}]),
    t(2, "(ab)",     [{seq,"ab"}]),
    t(3, "{a}",      [{par, "a"}]),
    t(4, "{(a)(b)}", [{par,[{seq,"a"},{seq,"b"}]}]),
    t(seq_of_chords, "{abc}{def}",
      [{par,"abc"},{par,"def"}]),
    t(loop, "(abcd)4*", [{loop,4,{seq,"abcd"}}]),
    t(sharp, "a#", [{sharp,$a}]),
    t(call, "[abc]", [{call,"abc"}]).

t(N, Str, Parse) ->
    case (catch ex(Str)) of
	Parse ->
	    io:format("test:~p OK~n", [N]);
	Other ->
	    io:format("*** test ~p failed~nInput     : ~p~n"
		      "Expecting : ~p~n" 
                      "Got       : ~p~n",
		      [N,Str, Parse, Other])
    end.

ex(Str) ->
    %% it's a stack machine with a list of objects on the stack
    e(Str, [#{dur=>8,shift=>0,vol=>80}], []).

%% a ...  S  => [a|S]

unwind([H|T], H, L) ->
    {L, T};
unwind([H|T], Stop, L) ->
    unwind(T,Stop, [H|L]).

e([$(|T], [D|Ds], L) ->
    %% make a new stack frame
    e(T, [D,D|Ds], [$(|L]);
e([$)|T], [_|E], L) ->
    %% pop the DSV stack (Duration, Shift, Vol)
    {X, Y} = unwind(L, $(, []),
    e(T, E, [{seq,X}|Y]);
e([$}|T], Dsv, L) ->
    {X, Y} = unwind(L, ${, []),
    e(T, Dsv, [{par,X}|Y]);
e([$[|T], Dsv, L) ->
    {Name, T1} = get_name(T, []),
    e(T1, Dsv, [{call,Name}|L]);

e([$*|T], Dsv, [{int,N},X|L])     -> e(T, Dsv, [{loop,N,X}|L]);
e([$T|T], [D|Dt], [{int,N}|L])    -> e(T, [D#{dur:=N}|Dt], L);
e([$V|T], [D|Dt], [{int,N}|L])    -> e(T, [D#{vol:=N}|Dt], L);
e([$<|T], [#{shift:=K}=D|Dt], L)  -> e(T, [D#{shift:= K-12}|Dt], L);
e([$>|T], [#{shift:=K}=D|Dt], L)  -> e(T, [D#{shift:= K+12}|Dt], L);

e([$-|T], Dsv, [H|L]) -> e(T, Dsv, [flatten(H, the_shift(Dsv))|L]);
e([$#|T], Dsv, [H|L]) -> e(T, Dsv, [sharpen(H, the_shift(Dsv))|L]);
e([$s|T], Dsv, [H|L]) -> e(T, Dsv, [{silence, the_duration(Dsv)}|L]);

e([H|T], Dsv, L) when ?IS_DIGIT(H) ->
    {Int, T1} = collect_int(T, H-$0),
    e(T1, Dsv, [{int,Int}|L]);
e([H|T], [D|_] = Dsv, L) when H >= $a, H =< $g ->
    #{dur := Dur, shift := S, vol := Vol} = D,
    e(T, Dsv, [{note,S+asci_to_midi(H), Dur, Vol}|L]);
e([$T|T], Dsv, [{int,N}|L]) ->
    %% tempo
    e(T, Dsv, [{tempo,T}|L]);
e([H|T], Dsv, L) ->
    io:format("*** H=~p~n",[H]),
    e(T, Dsv, [H|L]);
e([], _, L) ->
    reverse(L).

the_shift([#{shift := S}|_]) -> S.

the_duration([#{dur := D}|_]) -> D.

get_name([$]|T], L) -> {reverse(L), T};
get_name([H|T], L) -> get_name(T, [H|L]).

make_name([{int,N}|T]) -> integer_to_list(N) ++ make_name(T);
make_name([H|T])       -> [H|make_name(T)];
make_name([])          -> [].

sharpen({note,N,D,V}, S) -> {note,N+1,D,V};
sharpen({seq,L}, S)  -> {seq, [sharpen(I, S) || I <- L]}; 
sharpen({par, L}, S) -> {par, [sharpen(I, S) || I <- L]}.

flatten({note,N,D,V}, S) -> {note,N-1,D,V};
flatten({seq,L},S)  -> {seq, [flatten(I,S) || I <- L]}; 
flatten({par, L},S) -> {par, [flatten(I,S) || I <- L]}.

collect_int([H|T], N) when ?IS_DIGIT(H) ->
    collect_int(T, 10*N + H - $0);
collect_int(T, N) ->
    {N, T}.

%% X+ is sharp X- is flat >< up or down an octave
%% 86421 3/8 changes the time appicable
%% $ push . pop
%% (abc)    chord
%% {abc}*K  repeat K times
%% [foo]    call foo:

%% stack machine
%% a b c 
%% ( .....)      sequence
%% ( ... N < > ) sequence of tempo K
%%






	
