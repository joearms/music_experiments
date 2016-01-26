-module(osc).
-compile(export_all).

test() ->
    test1(["/foo",1000,-1,"hello",1.234,5.678]),
    test1(["/abc"]),
    test1(["/abc",1,2,3]),
    test1(["/abc",2,0,0]),
    test1(["/abc",2,"abc",0,2,3]).

%%     The next example shows the 40 bytes in the representation of the OSC Message
%% with OSC Address Pattern "/foo" and 5 arguments:

%% The int32 1000
%% The int32 -1
%% The string "hello"
%% The float32 1.234
%% The float32 5.678
%%  2f (/)  66 (f)  6f (o)  6f (o)
%%  0 ()    0 ()    0 ()    0 ()

%%  2c (,)  69 (i)  69 (i)  73 (s)
%%  66 (f)  66 (f)  0 ()    0 ()

%%  0 ()    0 ()    3 ()    e8 (è)
%%  ff (ÿ)  ff (ÿ)  ff (ÿ)  ff (ÿ)

%%  68 (h)  65 (e)  6c (l)  6c (l)
%%  6f (o)  0 ()    0 ()    0 ()

%%  3f (?)  9d ()   f3 (ó)  b6 (¶)
%%  40 (@)  b5 (µ)  b2 (”)  2d (-)

test1([H|T]=M) ->
    io:format("testing:~p~n",[M]),
    B1 = osc_lib:encode({message,H,T}),
    B2 = encode(M),
    case B1 of
	B2 ->
	    B2;
	_ ->
	    io:format("oops ~p~nlib:~p~n me:~p~n",[M,B1,B2])
    end,
    case decode(B2) of
	M ->
	    ok;
	M1 ->
	    io:format("input:~p~nrecon:~p~n",[M,M1])
    end,
    B2.

	    
encode([Verb|Args]) ->
    Str   = encode_arg(Verb),
    Flags = encode_flags(Args),
    Data  = [encode_arg(I) || I <- Args],
    list_to_binary([Str,Flags,Data]).


encode_arg(X) when is_list(X)    -> encode_string(X);
encode_arg(X) when is_atom(X)    -> encode_string(atom_to_list(X));
encode_arg(X) when is_integer(X) -> <<X:32>>;
encode_arg(X) when is_float(X)   -> <<X:32/float>>.


encode_flags(L) when is_list(L) ->
    %% flags starts with , and is terminated with a zero
    %% so it's really a string :-)
    L1 = [flag(I) || I <- L],
    encode_string([$,|L1]).

flag(I) when is_integer(I) -> $i;
flag(X) when is_list(X)    -> $s;
flag(X) when is_atom(X)    -> $s;
flag(X) when is_float(X)   -> $f.

encode_string(S) ->
    %% zero terminate S and pad to 4 byte boundary
    case length(S) rem 4 of
	0 -> [S,0,0,0,0];
	1 -> [S,0,0,0];
	2 -> [S,0,0];
	3 -> [S,0]
    end.

decode(B0) when is_binary(B0) ->    
    {Verb,  B1}      = get_string(B0),
    {[$,|Flags], B2} = get_string(B1),
    [Verb|get_args(Flags, B2, [])].

get_args([$i|T1], <<I:32/signed-integer,T2/binary>>, L) ->
    get_args(T1, T2, [I|L]);
get_args([$f|T1], <<F:32/float, T2/binary>>, L) ->
    get_args(T1, T2, [F|L]);
get_args([$d|T1], <<Double:64/float, T2/binary>>, L) ->
    get_args(T1, T2, [Double|L]);
get_args([$s|T1], B0, L) ->
    {Str, B1} = get_string(B0),
    get_args(T1, B1, [Str|L]);
get_args([], _, L) ->
    lists:reverse(L).
    
get_string(X) when is_binary(X) -> 
    [Bin,After] = binary:split(X, <<0>>),
    %% skip to bounday
    K = case size(Bin) rem 4 of
	    0 -> 3;
	    1 -> 2;
	    2 -> 1;
	    3 -> 0
	end,
    {binary_to_list(Bin), skip(K, After)}.

skip(0, B) -> B;
skip(N, B) -> element(2, erlang:split_binary(B, N)).
