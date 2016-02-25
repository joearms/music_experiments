-module(midi_event_gen).

-compile(export_all).

start(Type) ->
    Exec = driver(Type),
    Prog = filename:dirname(code:which(?MODULE)) ++ Exec,
    register(?MODULE, 
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Port = open_port({spawn, Prog},
					    [{packet, 2}]),
		     loop(Port)
		   end)),
    sleep(1500).  %% why since drivers takes a while to start

driver(internal) -> "/midi_synt_driver";
driver(external) -> "/midi_event_gen".

sleep(T) ->
    receive
	after T ->
		true
	end.

stop() ->
    ?MODULE ! stop.

send(M) -> call_port([1|M]).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{?MODULE, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {?MODULE, Data}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason})
    end.
	
