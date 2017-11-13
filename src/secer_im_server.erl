-module(secer_im_server).
-export([init/0]).
-record(
	state,
	{
		empty_trace = dict:new(), 
		valued_trace = dict:new(),
		
		timeouted_trace = dict:new(),

		same_trace = dict:new(),
		different_trace = dict:new(),

		relations,
		id_poi_dic,
		compare_fun = undef,
		cvg = 0
	}).

init() ->
	loop(#state{}).

loop(State) ->
	receive
		{set_rels,Relations,Dic} ->
			NewState = State#state
			{
				relations = Relations,
				id_poi_dic = Dic
			},
			loop(NewState);
		{set_fun,F} ->
			NewState = State#state
			{
				compare_fun = F
			},
			loop(NewState);
		{contained,Input,Ref,Pid} ->
			case {dict:is_key(Input,State#state.same_trace),dict:is_key(Input,State#state.different_trace)} of
				{false,false} ->
					Pid ! {Ref,false};
				_ -> 
					Pid ! {Ref,true}
			end,
			loop(State);
		{existing_trace,Input,Trace,Ref,Pid} ->
			case Trace of
				{[],[]} ->
					Pid ! {Ref,true};
				_ ->
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
					Pid ! {Ref,Exists}
			end,
			NewState =  
				State#state
				{
					valued_trace =
						dict:store(
							Input,
							Trace,
							State#state.valued_trace)
				},
			loop(NewState);
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
				timeouted ->
					State#state
					{
						timeouted_trace =
							dict:store(
								Input,
								Trace,
								State#state.timeouted_trace),
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
		{add,Input,Trace1,Trace2,independent} ->
			NewState = trace_division(Input,Trace1,Trace2,State),
			loop(NewState);
		{add,Input,Trace1,Trace2,_,Pid,Ref} -> 
			CompareRes = case State#state.compare_fun of
				undef ->
			 		compare_default(Trace1,Trace2,State);
			 	F ->
			 		compare_whole_trace(Trace1,Trace2,State#state.id_poi_dic,F)
			 		%compare_user(Trace1,Trace2,State,F)
			end,
			Pid ! {Ref,CompareRes},
			NewState = case CompareRes of
				true ->
					case Trace1 of
						[] ->
							State#state
							{
								empty_trace =
									dict:store(
										Input,
										{Trace1,Trace2},
										State#state.empty_trace),
								same_trace = 
									dict:store(
										Input,
										{Trace1,Trace2},
										State#state.same_trace)
							};
						_ ->
							State#state
							{
								% valued_trace =
								% 	dict:store(
								% 		Input,
								% 		{Trace1,Trace2},
								% 		State#state.valued_trace),
								same_trace = 
									dict:store(
										Input,
										{Trace1,Trace2},
										State#state.same_trace)
							}
					end;
				{error_no_relation,P1,P2} ->
					State#state
					{
						% valued_trace =
						% 	dict:store(
						% 		Input,
						% 		{Trace1,Trace2},
						% 		State#state.valued_trace),
						different_trace = 
							dict:store(
								Input,
								{Trace1,Trace2,"Unexpected trace order",P1,P2},
								State#state.different_trace)
					};
				{error_no_value,P1,P2} ->
					State#state
					{
						% valued_trace =
						% 	dict:store(
						% 		Input,
						% 		{Trace1,Trace2},
						% 		State#state.valued_trace),
						different_trace = 
							dict:store(
								Input,
								{Trace1,Trace2,"Unexpected trace value",P1,P2},
								State#state.different_trace)
					};
				{error,P1,P2} ->
					State#state
					{
						% valued_trace =
						% 	dict:store(
						% 		Input,
						% 		{Trace1,Trace2},
						% 		State#state.valued_trace),
						different_trace = 
							dict:store(
								Input,
								{Trace1,Trace2,"Error found",P1,P2},
								State#state.different_trace)
					};
				{false,Msg} ->
					State#state
					{
						% valued_trace =
						% 	dict:store(
						% 		Input,
						% 		{Trace1,Trace2},
						% 		State#state.valued_trace),
						different_trace = 
							dict:store(
								Input,
								{Trace1,Trace2,Msg,"User Defined","User Defined"},
								State#state.different_trace)
					};
				{false,Msg,P1,P2} ->
					State#state
					{
						% valued_trace =
						% 	dict:store(
						% 		Input,
						% 		{Trace1,Trace2},
						% 		State#state.valued_trace),
						different_trace = 
							dict:store(
								Input,
								{Trace1,Trace2,Msg,P1,P2},
								State#state.different_trace)
					};
				{different_length_trace,P1,P2} ->
					Msg = case {Trace1,Trace2} of
						{[],_} ->
							"The first trace is empty";
						{_,[]} ->
							"The second trace is empty";
						_ ->
							"The length of both traces differs"
					end,
					State#state
					{
						% valued_trace =
						% 	dict:store(
						% 		Input,
						% 		{Trace1,Trace2},
						% 		State#state.valued_trace),
						different_trace = 
							dict:store(
								Input,
								{Trace1,Trace2,Msg,P1,P2},
								State#state.different_trace)
					};
				_ ->
					printer("Unexpected error"),
					exit("Unexpected message")
			end,
			loop(NewState);
		{get_results,Pid} ->
			{Empty,Valued,Same,Different,Cvg} = 
				{State#state.empty_trace, State#state.valued_trace,
					State#state.same_trace,State#state.different_trace,State#state.cvg},
			Pid ! {Empty,Valued,Same,Different,Cvg,State#state.id_poi_dic};

		Other ->
			erlang:exit(
				self(), 
				{
					error, 
					{"Title: Error option.~n", Other}
				})
	end.

compare_default([],[],_) -> true;
compare_default([],[{Id2,Value2}|Trace2],S) -> 
	{ok,POI2} = dict:find(Id2,S#state.id_poi_dic),
	case equality({null,[]},{POI2,Value2},S#state.relations) of
		true ->
			compare_default([],Trace2,S);
		Error ->
			Error
	end;
compare_default([{Id1,Value1}|Trace1],[],S) -> 
	{ok,POI1} = dict:find(Id1,S#state.id_poi_dic),
	case equality({POI1,Value1},{null,[]},S#state.relations) of
		true ->
			compare_default(Trace1,[],S);
		Error ->
			Error
	end;
compare_default([{Id1,Value1}|Trace1],[{Id2,Value2}|Trace2],S) ->
	{ok,POI1} = dict:find(Id1,S#state.id_poi_dic),
	{ok,POI2} = dict:find(Id2,S#state.id_poi_dic),
	case equality({POI1,Value1},{POI2,Value2},S#state.relations) of
		true ->
			compare_default(Trace1,Trace2,S);
		Error ->
			Error
	end.

equality({null,_},{POI2,_},Rels) ->
	POI1s = lists:foldl(
		fun(E,RelPois) ->
			case E of
				{POI1,POI2} ->
					[POI1|RelPois];
				_ ->
					RelPois
			end
		end,
		[],
		Rels),
	POI = case POI1s of
		[POISingle] ->
			POISingle;
		_ ->
			POI1s
	end,
	{different_length_trace,POI,POI2};
	%{different_length_trace,POI1s,POI2};
equality({POI1,_},{null,_},Rels) ->
	POI2s = lists:foldl(
		fun(E,RelPois) ->
			case E of
				{POI1,POI2} ->
					[POI2|RelPois];
				_ ->
					RelPois
			end
		end,
		[],
		Rels),
	POI = case POI2s of
		[POISingle] ->
			POISingle;
		_ ->
			POI2s
	end,
	{different_length_trace,POI1,POI};
	%{different_length_trace,POI1,POI2s};
equality({POI1,Val1},{POI2,Val2},Rels) ->
	case lists:member({POI1,POI2},Rels) of 
		true ->
			case Val1 == Val2 of
				true ->
					true;
				false ->
					{error_no_value,POI1,POI2}
			end;
		_ ->
			{error_no_relation,POI1,POI2}
	end.

compare_whole_trace(T1,T2,Dic,F) ->
	%{ok,POI2} = dict:find(Id2,S#state.id_poi_dic),
	{NewT1,_} = lists:mapfoldl(
		fun({Id,V},D) ->
			{ok,POI} = dict:find(Id,D),
			{{POI,V},D}
		end,
		Dic,
		T1),
	{NewT2,_} = lists:mapfoldl(
		fun({Id,V},D) ->
			{ok,POI} = dict:find(Id,D),
			{{POI,V},D}
		end,
		Dic,
		T2),
	F(NewT1,NewT2).

% COMPARACION EN CADA ELEMENTO DE LA TRAZA (AUN NO IMPLEMENTADO EXPLICITAMENTE)
compare_user([],[],_,_) -> true;
compare_user([],[{Id2,Value2}|Trace2],S,F) ->
	{ok,POI2} = dict:find(Id2,S#state.id_poi_dic),
	
	case F([],{POI2,Value2}) of
		true ->
			compare_user([],Trace2,S,F);
		false ->
			{error,empty_trace,POI2};
		Error ->
			Error
	end;
compare_user([{Id1,Value1}|Trace1],[],S,F) ->
	{ok,POI1} = dict:find(Id1,S#state.id_poi_dic),
	
	case F({POI1,Value1},[]) of
		true ->
			compare_user(Trace1,[],S,F);
		false ->
			{error,POI1,empty_trace};
		Error ->
			Error
	end;
compare_user([{Id1,Value1}|Trace1],[{Id2,Value2}|Trace2],S,F) ->
	{ok,POI1} = dict:find(Id1,S#state.id_poi_dic),
	{ok,POI2} = dict:find(Id2,S#state.id_poi_dic),
	
	case F({POI1,Value1},{POI2,Value2}) of
		true ->
			compare_user(Trace1,Trace2,S,F);
		false ->
			{error,POI1,POI2};
		{false,ErrorMsg} ->
			{false,ErrorMsg,POI1,POI2}
	end.

trace_division(Input,T1,T2,S) ->
	Rels = S#state.relations,
	OldPois = lists:foldr(
					fun({E,_},Acc) ->
						case lists:member(E,Acc) of
							true ->
								Acc;
							false ->
								[E|Acc]
						end
					end,
					[],
					Rels),

	TransformedTraces = [identify_trace(T1,T2,PoiOld,S,Rels,[],[]) || PoiOld <- OldPois],
	lists:foldl(
		fun({Trace1,Trace2},State) -> 
			CompareRes = case State#state.compare_fun of
			undef ->
		 		compare_default(Trace1,Trace2,State);
		 	F ->
		 		compare_user(Trace1,Trace2,State,F)
			end,

			case CompareRes of
				true ->
					case dict:find(Input,State#state.same_trace) of
						{ok,Val} ->
							State#state
							{
								same_trace = 
									dict:store(
										Input,
										[Val]++[{Trace1,Trace2}],
										State#state.same_trace)
							};
						error ->
							State#state
							{
								same_trace = 
									dict:store(
										Input,
										{Trace1,Trace2},
										State#state.same_trace)
							}
					end;
				{error_no_value,P1,P2} ->
					case dict:find(Input,State#state.different_trace) of
						{ok,Val} ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										[Val]++[{Trace1,Trace2,"Unexpected trace value",P1,P2}],
										State#state.different_trace)
							};
						error ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										{Trace1,Trace2,"Unexpected trace value",P1,P2},
										State#state.different_trace)
							}
					end;
				{error,P1,P2} ->
					case dict:find(Input,State#state.different_trace) of
						{ok,Val} ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										[Val]++[{Trace1,Trace2,"Error found",P1,P2}],
										State#state.different_trace)
							};
						error ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										{Trace1,Trace2,"Error found",P1,P2},
										State#state.different_trace)
							}
					end;
				{false,Msg} ->
					case dict:find(Input,State#state.different_trace) of
						{ok,Val} ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										[Val]++[{Trace1,Trace2,Msg}],
										State#state.different_trace)
							};
						error ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										{Trace1,Trace2,Msg},
										State#state.different_trace)
							}
					end;
				{different_length_trace,P1,P2} ->
					case dict:find(Input,State#state.different_trace) of
						{ok,Val} ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										[Val]++[{Trace1,Trace2,"The length of both traces differs",P1,P2}],
										State#state.different_trace)
							};
						error ->
							State#state
							{
								different_trace = 
									dict:store(
										Input,
										{Trace1,Trace2,"The length of both traces differs",P1,P2},
										State#state.different_trace)
							}
					end;
				_ ->
					exit("Unexpected message")
			end
		end,
		S,
		TransformedTraces).
		
identify_trace([],[],_,_,_,T1Rel,T2Rel) -> {lists:reverse(T1Rel),lists:reverse(T2Rel)};
identify_trace([{Id1,Val1}|T1],[],Poi,S,Rels,T1Rel,T2Rel) -> 
	{ok,POI1} = dict:find(Id1,S#state.id_poi_dic),
	case POI1 of
		Poi ->
			identify_trace(T1,[],Poi,S,Rels,[{Id1,Val1}|T1Rel],T2Rel);
		_ ->
			identify_trace(T1,[],Poi,S,Rels,T1Rel,T2Rel)
	end;
identify_trace([],[{Id2,Val2}|T2],Poi,S,Rels,T1Rel,T2Rel) -> 
	{ok,POI2} = dict:find(Id2,S#state.id_poi_dic),
	case lists:member({Poi,POI2},Rels) of
		true ->
			identify_trace([],T2,Poi,S,Rels,T1Rel,[{Id2,Val2}|T2Rel]);
		false ->
			identify_trace([],T2,Poi,S,Rels,T1Rel,T2Rel)
	end;
identify_trace([{Id1,Val1}|T1],[{Id2,Val2}|T2],Poi,S,Rels,T1Rel,T2Rel) ->
	{ok,POI1} = dict:find(Id1,S#state.id_poi_dic),
	case POI1 of
		Poi -> 
			{ok,POI2} = dict:find(Id2,S#state.id_poi_dic),
			case lists:member({Poi,POI2},Rels) of
				true ->
					identify_trace(T1,T2,Poi,S,Rels,[{Id1,Val1}|T1Rel],[{Id2,Val2}|T2Rel]);
				false ->
					identify_trace([{Id1,Val1}|T1],T2,Poi,S,Rels,T1Rel,T2Rel)		
			end;
		_ ->
			identify_trace(T1,[{Id2,Val2}|T2],Poi,S,Rels,T1Rel,T2Rel)
	end.

printer(Node) -> io:format("~p\n",[Node]).
