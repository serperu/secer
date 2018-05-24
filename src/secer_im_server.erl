-module(secer_im_server).
-export([init/0]).
-record(
	state,
	{	
		timeouted_trace = dict:new(),

		error_dict = dict:new(), % Esto posiblemente vuele cuando se trate el UB Report

		same_trace = dict:new(),
		different_trace = dict:new(), %KEY: Input
		trace_dict = dict:new(),	  %KEY: Trace

		cfun = secer_cfuns:cf_general(),

		cfun_changed = false, % Mirar como eliminar esto

		id_relations,
		poi_relations,
		id_poi_dic
	}).

init() ->
	loop(#state{}).

loop(State) ->
	receive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INITIALIZE FEATURES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		{set_rels,IdRelations,POIRelations,Dic} ->
			NewState = State#state
			{
				id_relations = IdRelations,
				poi_relations = POIRelations,
				id_poi_dic = Dic
			},
			loop(NewState);
		{set_cfun,CFUN} ->
			NewState = State#state
				{ 
					cfun = CFUN,
					cfun_changed = true
				},
			loop(NewState);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREVIOSLY GENERATED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		{contained,Input,Ref,Pid} ->
			case {dict:is_key(Input,State#state.same_trace),
					dict:is_key(Input,State#state.different_trace)} of
				{false,false} ->
					Pid ! {Ref,false};
				_ -> 
					Pid ! {Ref,true}
			end,
			loop(State);
		{existing_trace,_,Trace,Ref,Pid} ->
			case Trace of
				{[],[]} ->
					Pid ! {Ref,true};
				_ ->
					Exists = dict:is_key(Trace,State#state.trace_dict),
					Pid ! {Ref,Exists}
			end,
			loop(State);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TIMEOUTED TRACES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		{add,Input,timeouted,Trace2,_} ->
			NewState = State#state
				{
					timeouted_trace =
						dict:store(
							Input,
							{timeouted,Trace2},
							State#state.timeouted_trace)
				},
			loop(NewState);
		{add,Input,Trace1,timeouted,_} ->
			NewState = State#state
				{
					timeouted_trace =
						dict:store(
							Input,
							{Trace1,timeouted},
							State#state.timeouted_trace)
				},
			loop(NewState);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CATEGORIZATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		{add,Input,Trace1,Trace2,Pid,Ref} -> 
			% printer("Trace1"),
			% printer(Trace1),
			% printer("Trace2"),
			% printer(Trace2),
			CompareRes = case State#state.cfun_changed of
				true ->
					secer_cfuns:undo_id_traces(Trace1,Trace2, State#state.id_poi_dic),
					(State#state.cfun)(Trace1, Trace2, State#state.poi_relations);
				_ ->
					(State#state.cfun)(Trace1, Trace2, State#state.id_relations,State#state.id_poi_dic)
			end,		
			
			Pid ! {Ref,CompareRes},
			NewState = case CompareRes of
				true ->
					State#state
					{
						same_trace = 
							dict:store(
								Input,
								{Trace1,Trace2},
								State#state.same_trace)
					};
				{false,ErrorType,ErrorInfo} ->
					State#state
					{
						different_trace = 
							dict:store(
								Input,
								{ErrorType,ErrorInfo},
								State#state.different_trace),
						% error_dict = 
						% 	dict:append( %Este append penaliza lo suyo con respecto al store
						% 		ErrorType,
						% 		Input,
						% 		State#state.error_dict),
						trace_dict = 
							dict:store(
								{Trace1,Trace2},
								0,
								State#state.trace_dict)
					};
				X ->
					io:formaT("~p\n",[X]),
					io:format("~s\n",["Unexpected error"]),
					exit("Unexpected message")
			end,
			loop(NewState);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RESULT SENDING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		{get_results,Pid} ->
			Pid ! {State#state.same_trace,State#state.different_trace,%State#state.error_dict,
				 	State#state.id_poi_dic,State#state.timeouted_trace};

		Other ->
			erlang:exit(
				self(), 
				{
					error, 
					{"Title: Error option.~n", Other}
				})
	end.
undo_id_traces([],[],IdDict,NewL1,NewL2) ->
	{lists:reverse(NewL1),lists:reverse(NewL2)};
undo_id_traces([{P1,V1}|L1],[],IdDict,NewL1,NewL2) ->
	undo_id_traces(L1,[],IdDict,[{dict:fetch(P1,IdDict),V1} | NewL1],NewL2);
undo_id_traces([],[{P2,V2}|L2],IdDict,NewL1,NewL2) ->
	undo_id_traces([],L2,IdDict,NewL1,[{dict:fetch(P2,IdDict),V2} | NewL2]);
undo_id_traces([{P1,V1}|L1],[{P2,V2}|L2],IdDict,NewL1,NewL2) ->
	undo_id_traces(L1,L2,IdDict,[{dict:fetch(P1,IdDict),V1} | NewL1],[{dict:fetch(P2,IdDict),V2} | NewL2]).



printer(Node) -> io:format("~p\n",[Node]).
