-module(secer_fv_server). 
-export([init/0]).
-record(
	state,
	{
		variables = sets:new(), 
		max_length_variable = "", 
		current_id = 0
	}).

init() ->
	loop(#state{}).

loop(State) ->
	receive
		exit ->
			io:format("---Ejecucion Correcta---\n"),
			ok;
		error ->
			io:format("---Ejecucion Con Errores---\n"),
			ok;
		reset ->
			init();
		{add_variable, Variable} ->
			NewState = 
				State#state
				{
					variables = 
						sets:add_element(
							Variable, 
							State#state.variables)
				},
			loop(NewState);
		all_variables_added ->
			NewState = 
				State#state
				{
					variables = 
						sets:new(),
					max_length_variable = 
						sets:fold(
							fun get_max_length_variable/2,
							"",
							State#state.variables)
				},
			loop(NewState);
		{get_free_variable, Ref, Pid} ->
			CurrentId = 
				State#state.current_id,
			NewState =
				State#state{
					current_id = 
						CurrentId + 1
				},
			FreeVariable = 
					State#state.max_length_variable 
				++ 	integer_to_list(CurrentId), 

			Pid ! {erl_syntax:variable(FreeVariable),Ref},
			loop(NewState);
		Other ->
			erlang:exit(
				self(), 
				{
					error, 
					{"Title: Error option.~n", Other}
				})
	end.

get_max_length_variable(Variable, Max) 
		when length(Variable) > length(Max) ->
	Variable;
get_max_length_variable(_, Max) ->
	Max.
