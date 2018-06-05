-module(secer_cfuns).
-export([cf_general/0, cf_general/1, cf_independent/0, cf_independent/1]).
-export([equals/2, different/2, lower_than/2, greater_than/2]).
%-export([undo_id_transformation/4]).

%-export([show/0, comp_perf/1, lists_comp_perf/1]). % PENDING
-record(
	config, 
	{	
		vef = fun({_, V, _}) -> V end, 
		tecf = fun(VEF, TOE, TNE, CFUN) ->  	% Esto existe porque cuando haya additional information 
					CFUN(VEF(TOE), VEF(TNE)) % habra que hacer algo más que llamar a la CFUN. 
			   end, 							% Habrá que mirar la AI para categorizar más errores
		cfun = fun secer_cfuns:equals/2, %fun secer_cfuns:equals/2
		
		ubrm = dict:new()%([{default_error, fun secer_report_constructor:default_report/3}]) 
		%FALTA añadir esta entrada al UBRM del usuario
	}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% GENERAL COMPARISON FUNCTION %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Estas funciones deben estar en cfuns porque permiten que el usuario pueda definir funciones 
% que utilizen los valores de todas las trazas generadas y extraigan datos sobre el total.
cf_general() ->
	cf_general(#config{}).

cf_general(#config{vef=VEF, tecf=TECF, cfun=CFUN, ubrm=UBRM}) ->
	fun(TO, TN, PoiRel, InternalIdDict) ->
		UBMRWithDefault = dict:store(default_error, fun secer_report_constructor:default_report/3,UBRM),
		cf_general(TO, TN, VEF, TECF, CFUN, UBMRWithDefault, PoiRel, InternalIdDict)
	end.

cf_general(TO, TN, VEF, TECF, CFUN, UBRM, PoiRel, InternalIdDict) ->  
	cf_general(TO, TN, VEF, TECF, CFUN, UBRM, PoiRel, InternalIdDict, []).

cf_general([], [], _, _, _, _, _, _, _) -> 
	true;
cf_general([TOE | _], [], _, _, _, _, _, _, His) -> 
	{false, first_trace_longer, {TOE, undefined, lists:reverse(His)}};
cf_general([], [TNE | _], _, _, _, _, _, _, His) -> 
	{false, second_trace_longer, {undefined, TNE, lists:reverse(His)}};
cf_general([TOE | TO], [TNE | TN], VEF, TECF, CFUN, UBRM, PoiRel, InternalIdDict, His) -> 
	case related_pois(TOE, TNE, PoiRel) of
		true ->
			case TECF(VEF, TOE, TNE, CFUN) of
				true ->
					cf_general(TO, TN, VEF, TECF, UBRM, PoiRel, InternalIdDict, [{TOE , TNE} | His]);
				UBT -> {
					false, 
					UBT, 
					fun() ->
						my_fetch(UBT, UBRM, TOE, TNE, lists:reverse(His), InternalIdDict)
					end
					} 
			end;
		UBT ->
			{false, UBT, {TOE, TNE, lists:reverse(His)}}
	end.

related_pois({POI1, _, _}, {POI2, _, _}, PoiRel) ->
	case lists:member({POI1, POI2}, PoiRel) of
		true -> true;
		false -> no_poi_relation
	end.

my_fetch(UBT, UBRM, TOE, TNE, His, InternalIdDict) ->
	{NewTOE, NewTNE, NewHis} = undo_id_transformation(TOE, TNE, His, InternalIdDict), 
	case dict:find(UBT, UBRM) of
		{ok, ReportFunction} -> 
			(ReportFunction)(NewTOE, NewTNE, NewHis);
		error ->
			(dict:fetch(default_error, UBRM))(NewTOE, NewTNE, NewHis)
	end.

undo_id_transformation({POld, VOld, AIOld}, {PNew, VNew, AINew}, His, IdDict) ->
	{
		{dict:fetch(POld, IdDict), VOld, AIOld}, 
		{dict:fetch(PNew, IdDict), VNew, AINew}, 
		lists:map(fun({{PO, VO, AIO}, {PN, VN, AIN}}) ->
					{{dict:fetch(PO, IdDict), VO, AIO}, {dict:fetch(PN, IdDict), VN, AIN}}
				  end, 
				  His)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% INDEPENDENT MODE %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cf_independent() ->
	cf_independent(#config{}).

cf_independent(#config{vef=VEF, tecf=TECF, cfun=CFUN, ubrm=UBRM}) ->
	fun(TO, TN, PoiRel, InternalIdDict) ->
		UBMRWithDefault = dict:store(default_error, fun secer_report_constructor:default_report/3,UBRM),
		cf_independent(TO, TN, VEF, TECF, CFUN, UBMRWithDefault, PoiRel, InternalIdDict)
	end.

% CUIDADO: Esto devuelve una lista: [true | {false, UBT, {P1, P2, His}}]
cf_independent(TO, TN, VEF, TECF, CFUN, UBRM, PoiRel, InternalIdDict) -> 
	TraceList = trace_division(TO, TN, PoiRel), 
	lists:map(fun({TOIndependent, TNIndependent}) ->
				cf_general(TOIndependent, TNIndependent, VEF, TECF, CFUN, UBRM, PoiRel, InternalIdDict, [])
			  end, 
			  TraceList).

trace_division(T1, T2, PoiRels) ->
	RelSets = rel_association(PoiRels, []), 
	lists:map(fun(RS) ->
				{Set1, Set2} = RS, 
				SetT1 = lists:foldr(fun({POI, V, AI}, RT) -> 
								case lists:member(POI, Set1) of
									true -> [{POI, V, AI}|RT];
									_ -> RT
								end
							end, 
							[], 
							T1), 
				SetT2 = lists:foldr(fun({POI, V, AI}, RT) -> 
								case lists:member(POI, Set2) of
									true -> [{POI, V, AI}|RT];
									_ -> RT
								end
							end, 
							[], 
							T2), 
				{SetT1, SetT2}
			  end, 
			  RelSets).

rel_association([], Sets) ->
	lists:reverse(Sets);
rel_association([{P1, P2} | T], Sets) ->
	{NewT, RN} = lists:foldl(fun({E1, E2}, {NT, L}) -> 
								case (E1 == P1) of
									true -> {lists:delete({E1, E2}, NT), [E2 | L]};
									_ -> {NT, L}
								end
			 				end, 
						 	{T, []}, 
						 	T), 
	RelNew = [P2 | RN], 
	{FinalT, RO} = lists:foldl(fun({E1, E2}, {NT, L}) -> 
								case (E2 == P2) of
									true -> {lists:delete({E1, E2}, NT), [E1 | L]};
									_ -> {NT, L}
								end
						 	  end, 
						 	  {NewT, []}, 
						 	  NewT), 
	RelOld = [P1 | RO], 
	rel_association(FinalT, [{RelOld, RelNew} | Sets]).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% FUNTIONS USED INSIDE TECF %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
equals(Elem1, Elem2) ->
	case  Elem1 == Elem2 of
	   	true -> true;
	   	false -> different_value
	end.

different(Elem1, Elem2) ->
	case  Elem1 /= Elem2 of
	   	true -> true;
	   	false -> equal_value
	end.

lower_than(Elem1, Elem2) -> 
	case Elem1 > Elem2 of
		true->
			true;
		false when Elem1 == Elem2 ->
			equal_value;
		_ ->
			greater_value
	end.

greater_than(Elem1, Elem2) -> 
	case Elem1 < Elem2 of
		true->
			true;
		false when Elem1 == Elem2 ->
			equal_value;
		_ ->
			lower_value
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: REVIEW THE REST OF THE FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
comp_perf(Thresh) -> 
	fun(TO, TN, PoiRel) -> 
		comp_perf(TO, TN, PoiRel, Thresh) 
	end.

comp_perf(TO, TN, _, Thresh) ->
    ZippedList = lists:zip(TO, TN), 
    lists:foldl(
        fun
            (_, {false, Msg, P1, P2}) ->
                {false, Msg, P1, P2};
            ({{_, VO}, {_, VN}}, _) when (VO + Thresh) >= VN ->
                true;
            ({{P1, _}, {P2, _}}, _) ->
                {false, "Slower Calculation", P1, P2}
        end, 
        true, 
        ZippedList).

show() ->
	fun secer_cfuns:show/3.
show(TO, TN, _) -> 
	FunShowTrace = 
 		fun(T) ->
 			lists:map(
 				fun({POI, Value}) ->
 					io:format("POI: ~p\nValue: ~p\n", [POI, Value])
 				end, 
 			T)
		end, 
	io:format("Trace old version:\n"), 
	FunShowTrace(TO), 
	io:format("Trace new version:\n"), 
	FunShowTrace(TN), 
	true.     

lists_comp_perf(Threshold) -> 
	fun (TO, TN, PoiRel) -> 
		lists_comp_perf(TO, TN, PoiRel, Threshold) 
	end.
lists_comp_perf(TO, TN, PoiRel, Threshold) -> 
  FunWhenTrue = 
  	fun(_, _, _) ->
  		ok
  	end, 
  lists_comp_perf_common(TO, TN, PoiRel, Threshold, FunWhenTrue).

io_lists_comp_perf(TO, TN, PoiRel, Threshold) -> 
  FunWhenTrue = 
  	fun(I, VO, VN) ->
  		io:format(
  			"Faster Calculation: Length: ~p -> ~p vs ~p ms.\n", 
  			[length(I), VO, VN])
  	end, 
  lists_comp_perf_common(TO, TN, PoiRel, Threshold, FunWhenTrue).

lists_comp_perf_common(TO, TN, _, Threshold, FunWhenTrue) -> 
  ZippedList = lists:zip(TO, TN), 
  lists:foldl(
    fun
      (_, {false, Msg, P1, P2}) ->
        {false, Msg, P1, P2};
      ({{_, {_, VO}}, {_, {_, VN}}}, _) when abs(VO - VN) =< Threshold -> 
         true;
      ({{_, {I, VO}}, {_, {I, VN}}}, _) when VO >= VN ->
      	 FunWhenTrue(I, VO, VN), 
         true;
      ({{PO, {I, VO}}, {PN, {I, VN}}}, _) ->
         {
            false, 
            lists:flatten(
              io_lib:format(
                "Slower Calculation: Length: ~p -> ~p vs ~p µs.", 
                [length(I), VO, VN])), 
            PO, PN
          }
    end, 
    true, 
    ZippedList).

printer(Node) -> io:format("~p\n", [Node]).