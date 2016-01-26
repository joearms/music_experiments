-module(pd).
-compile(export_all).

%% start PD with the program
%% pd_sc.pd - click on DSP and "1" (turn sound on)

play(N) -> run_code(["/playNote", N]).

run_code(M) ->
    E = osc:encode(M),
    {ok, Socket} = gen_udp:open(0,[binary]),
    ok = gen_udp:send(Socket, "localhost", 6677, E),
    gen_udp:close(Socket).
   
    
    

