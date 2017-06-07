-module(secer_im_server).
-export([init/0]).
-record(
	state,
	{
		empty_trace = dict:new(), 
		valued_trace = dict:new(),
		cvg = 0
	}).

init() ->
	loop(#state{}).

loop(State) ->
	receive
		{contained,Input,Pid} ->
			case {dict:is_key(Input,State#state.empty_trace),dict:is_key(Input,State#state.valued_trace)} of
				{false,false} ->
					Pid ! false;
				_ -> 
					Pid ! true
			end,
			loop(State);
		{existing_trace,Trace,Pid} ->
			Exists = dict:fold(
				fun (_,V,Acc) ->
					case Acc of
						false ->
							V == Trace;
						true ->
							true
					end
				end,
				false,
				State#state.valued_trace),
			Pid ! Exists,
			loop(State);
		{add,Input,Trace,Cvg} ->
			NewState = case Trace of
				[] ->
					State#state
					{
						empty_trace =
							dict:store(
								Input,
								Trace,
								State#state.empty_trace),
						cvg = Cvg
					};
				_ ->
					State#state
					{
						valued_trace =
							dict:store(
								Input,
								Trace,
								State#state.valued_trace),
						cvg = Cvg
					}
			end,
			loop(NewState);
		{get_results,Pid} ->
			{Empty,Valued,Cvg} = {State#state.empty_trace,State#state.valued_trace,State#state.cvg},
			Pid ! {Empty,Valued,Cvg};

		Other ->
			erlang:exit(
				self(), 
				{
					error, 
					{"Title: Error option.~n", Other}
				})
	end.