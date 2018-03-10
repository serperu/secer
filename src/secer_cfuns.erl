-module(secer_cfuns).
-export([lower_than/3,greater_than/3,unequal/3,show/3]).

lower_than(TO,TN,PoiRel) -> 
	ZippedList = lists:zip(TO,TN),
	lists:foldl(
		fun
			(_,{false,Msg,P1,P2}) ->
				{false,Msg,P1,P2};
			({{PO,VO},{PN,VN}},true) ->
				case lists:member({PO,PN},PoiRel) of
					true when VO > VN ->
						true;
					true when VO =< VN ->
						{false,"Old Version Value < New Version Value",PO,PN};
					flase ->
						{false,"Unexpected trace order",PO,PN}
				end
		end,
		true,
		ZippedList).

greater_than(TO,TN,PoiRel) -> 
	ZippedList = lists:zip(TO,TN),
	lists:foldl(
		fun
			(_,{false,Msg,P1,P2}) ->
				{false,Msg,P1,P2};
			({{PO,VO},{PN,VN}},true) ->
				case lists:member({PO,PN},PoiRel) of
					true when VO < VN ->
						true;
					true when VO >= VN ->
						{false,"Old Version Value > New Version Value",PO,PN};
					flase ->
						{false,"Unexpected trace order",PO,PN}
				end
		end,
		true,
		ZippedList).

unequal(TO,TN,PoiRel) ->
	ZippedList = lists:zip(TO,TN),
	lists:foldl(
		fun
			(_,{false,Msg,P1,P2}) ->
				{false,Msg,P1,P2};
			({{PO,VO},{PN,VN}},true) ->
				case lists:member({PO,PN},PoiRel) of
					true when VO /= VN ->
						true;
					true when VO == VN ->
						{false,"The two elements are equal",PO,PN};
					flase ->
						{false,"Unexpected trace order",PO,PN}
				end
		end,
		true,
		ZippedList).

show(TO,TN,_) -> 
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

lists_comp_perf(TO, TN, _, Threshold) -> 
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
  ZippedList = lists:zip(TO,TN),
  lists:foldl(
    fun
      (_,{false,Msg}) ->
        {false,Msg};
      ({{_,{_, VO}},{_,{_, VN}}},_) when abs(VO - VN) =< Threshold -> 
         true;
      ({{_,{I, VO}},{_,{I, VN}}},_) when VO >= VN ->
      	 FunWhenTrue(I, VO, VN),
         true;
      ({{_,{I, VO}},{_,{I, VN}}}, _) ->
         {
            false,
            lists:flatten(
              io_lib:format(
                "Slower Calculation: Length: ~p -> ~p vs ~p µs.", 
                [length(I), VO, VN]))
          }
    end,
    true,
    ZippedList).


% ESTA FUNCION ACUMULA LAS TRAZAS DE POIS RELACIONADOS CON EL QUE DIO ERROR
% relation_verifier(PO,TO,PN,TN,T1,T2,Msg,P1,P2,PoiRel) -> 
% 	case {PO,PN} of
% 		{P1,P2} ->
% 			{false,T1++[TO],T2++[TN],Msg,PO,PN};
% 		{P1,POI} ->
% 			case lists:member({P1,POI},PoiRel) of
% 				true ->
% 					{false,T1++[TO],T2++[TN],Msg,PO,PN};
% 				false ->
% 					{false,T1++[TO],T2,Msg,PO,PN}
% 			end;
% 		{POI,P2} ->
% 			case lists:member({POI,P2},PoiRel) of
% 				true ->
% 					{false,T1++[TO],T2++[TN],Msg,PO,PN};
% 				false ->
% 					{false,T1,T2++[TN],Msg,PO,PN}
% 			end;
% 		_ ->
% 			{false,T1,T2,Msg,P1,P2}
% 		% {POI1,POI2} ->	
% 		% 	case {lists:member({P1,POI2},PoiRel),lists:member({POI1,P2},PoiRel)} of
% 		% 		{true,true} ->
% 		% 			{false,T1++TO,T2++TN,Msg,PO,PN};
% 		% 		{false,true} ->
% 		% 			{false,T1,T2++TN,Msg,PO,PN};
% 		% 		{true,false} ->
% 		% 			{false,T1++TO,T2,Msg,PO,PN};;
% 		% 		_ ->
% 		% 			{false,T1,T2,Msg,P1,P2}
% 		% 	end
% 	end.