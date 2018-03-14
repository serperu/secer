-module(secer_trace). 
-export([init/0]).
		
init() ->
	loop([]).

loop(State) ->
	receive
		exit ->
			ok;
		error ->
			io:format("---Error Execution---\n"),
			ok;
		reset ->
			loop([]);
		{add,Elem} ->
			NewState = [Elem|State],
			loop(NewState);
		{get_results,Ref,Pid} ->
			Results = lists:reverse(State),
			Pid ! {Ref,Results},
			loop([]);
		_ ->
			loop(State)
	end.
