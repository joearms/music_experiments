-module(sonic).
-compile(export_all).

%% start sonic pi 
%% then run me

%% to UDP port 4557
%% osc messages "/run_code" "play 50\n"

test1() ->
    run_code("use_synth :beep\nplay 50\n").

test2() ->
    run_code(["use_synth :fm\n",  make_scale()]).

make_scale() ->
    for(70,75, 
       fun(I) ->
	       ["play ",integer_to_list(I),"\n",
		"sleep 0.1\n"]
       end).

for(Max,Max,F) -> [F(Max)];
for(I,Max,F)   -> [F(I)|for(I+1,Max,F)].

run_code(Prog) ->
    %% Prog is a io-list
    P1 = lists:flatten(Prog),
    io:format("run-code~n~s~n",[P1]),
    M = ["/run-code" , "erl-id", P1],
    E = osc:encode(M),
    io:format("Decode=~p~n",[osc:decode(E)]),
    {ok, Socket} = gen_udp:open(0,[binary]),
    io:format("socket=~p message=~p~n",[Socket,E]),
    ok = gen_udp:send(Socket, "localhost", 4557, E),
    %% io:format("V=~p~n",[V]),
    gen_udp:close(Socket).
   
    
    

