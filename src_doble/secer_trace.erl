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
		{add,Elem} ->
			NewState = [Elem|State],
			loop(NewState);
		{get_results,Pid} ->
			Results = lists:reverse(State),
			Pid ! Results,
			loop([]);
		_ ->
			loop(State)
	end.