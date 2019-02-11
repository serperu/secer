-module(secer_api).
-export([nuai_config/0, nuai_t_config/1, nuai_r_config/0, nuai_r_config/1, 
		 nuai_tr_config/2, uai_config/0, uai_config/1, uai_config/2]).
-export([build_config/1, build_config/2, build_tecf/2]).
-export([cf_general/0, cf_general/1, cf_independent/0, cf_independent/1]).
-export([build_vef/0, build_vef/1, build_ubrm/0, build_ubrm/1]).
-export([vef_value_only/0, vef_st_only/0, vef_ca_only/0, vef_callee_only/0, vef_args_only/0, vef_poi_value/0, 
		 vef_poi_st/0, vef_poi_ca/0, vef_poi_callee/0, vef_poi_args/0, vef_value_st/0, vef_value_ca/0, 
		 vef_value_callee/0, vef_value_args/0, vef_poi_value_st/0, vef_poi_value_ca/0, vef_poi_value_callee/0,
		 vef_poi_value_args/0, vef_full/0]).
-export([get_te_st/1, get_te_ca/1, get_te_callee/1, get_te_args/1]).
-export([get_st/1, get_ca/1, get_callee/1, get_args/1]).
-export([equals/0, different/0, lower_than/0, greater_than/0]).
-export([equals/2, different/2, lower_than/2, greater_than/2]).
-record(
	config, 
	{	
		tecf,
		ubrm
	}).
-record(
	tecf_config,
	{
		vef = fun({_, V, _}) -> V end, 	
		cfun = fun secer_api:equals/2
	}).
%%%%%%%%%%%%%%%%%%%%%
%% DEFAULT CONFIGS %%
%%%%%%%%%%%%%%%%%%%%%
%% NUAI MODES %%
%%%%%%%%%%%%%%%%
nuai_config() ->
	build_config([]).

nuai_t_config(TECF) ->
	build_config(TECF).

nuai_r_config() ->
	build_config([]).

nuai_r_config(UBRM) ->
	build_config(UBRM).	

nuai_tr_config(TECF,UBRM) ->
	build_config(TECF, UBRM).	

%%%%%%%%%%%%%%%
%% UAI MODES %%
%%%%%%%%%%%%%%%
uai_config() ->
	build_config(build_tecf(vef_full(),[])).

uai_config(Conf) ->
	build_config(Conf).

uai_config(TECF, UBRM) ->
	build_config(TECF, UBRM).

%%%%%%%%%%%%%%%
%% AIT MODES %%
%%%%%%%%%%%%%%%

% PENDING (Special instrumentation)

%%%%%%%%%%%%%%%%%%%%%%%%%
%% BUILD CONFIGURATION %%
%%%%%%%%%%%%%%%%%%%%%%%%%
build_config(Config) ->
	case is_function(Config) of
		true ->
			build_config(Config,[]);
		false ->
			build_config([],Config)
	end.

build_config([],UBRM) ->
	#config{tecf = build_tecf(), ubrm = UBRM};
build_config(TECF,[]) ->
	#config{tecf = TECF, ubrm = []};
build_config(TECF, UBRM) ->
	#config{tecf = TECF, ubrm = UBRM}.

%%%%%%%%%%%%%%%%
%% BUILD TECF %%
%%%%%%%%%%%%%%%%
build_tecf() ->
	build_tecf(#tecf_config{}).

build_tecf(VEF, CFUN) ->
	case {is_function(VEF), is_function(CFUN)} of
		{true, true} ->
			build_tecf(#tecf_config{vef = VEF, cfun = CFUN});
		{true, false} ->
			build_tecf(#tecf_config{vef = VEF});
		{false, true} ->
			build_tecf(#tecf_config{cfun = CFUN});
		_ ->
			build_tecf(#tecf_config{})
	end.

build_tecf(#tecf_config{vef = VEF, cfun = CFUN}) ->
	fun(TOE, TNE) ->  	
		CFUN(VEF(TOE), VEF(TNE))
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BUILD VALUE EXTRACTOR FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_vef() ->	
	build_vef([]).

build_vef([]) ->
	fun({_, V, _}) -> V end;
build_vef(L) ->		% RETURNS A FUN RETURNING A LIST WITH THE SPECIFIED ITEMS
	fun({P, V, AI}) ->
		lists:foldr(
			fun(E,Acc) ->
				case E of
					poi ->
						[P | Acc];
					val ->
						[V | Acc];
					_ ->
						case dict:find(E,AI) of
							{ok, EValue} ->
								[EValue | Acc];
							error ->
								io:format("Unexistent Additional Information"),
								Acc	
						end
				end
			end,
			[],
			L)
	end.

%%%%%%%%%%%%%%%%%%%
%% VEF TEMPLATES %%
%%%%%%%%%%%%%%%%%%%
vef_value_only() -> 
	fun({_, V, _}) -> V end.
vef_st_only() ->
	fun({_, _, AI}) -> dict:fetch(st, AI) end.
vef_ca_only() ->
	fun({_, _, AI}) -> 
		case dict:find(ca, AI) of
			{ok, CA} ->
				CA;
			error ->
				none
		end
	end.
vef_callee_only() ->
	fun({_, _, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [Callee | _ ]} ->
				Callee;
			error ->
				none
		end
	end.
vef_args_only() ->
	fun({_, _, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [_ | Args]} ->
				Args;
			error ->
				none
		end
	end.
vef_poi_value() -> 
	fun({P, V, _}) -> {P, V} end.
vef_poi_st() ->
	fun({P, _, AI}) -> {P, dict:fetch(st, AI)} end.
vef_poi_ca() ->
	fun({P, _, AI}) -> 
		case dict:find(ca, AI) of
			{ok, CA} ->
				{P, CA};
			error ->
				{P, none}
		end
	end.
vef_poi_callee() ->
	fun({P, _, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [Callee | _]} ->
				{P, Callee};
			error ->
				{P, none}
		end
	end.
vef_poi_args() ->
	fun({P, _, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [_ | Args]} ->
				{P, Args};
			error ->
				{P, none}
		end
	end.
vef_value_st() ->
	fun({_, V, AI}) -> {V, dict:fetch(st, AI)} end.
vef_value_ca() ->
	fun({_, V, AI}) -> 
		case dict:find(ca, AI) of
			{ok, CA} ->
				{V, CA};
			error ->
				{V, none}
		end
	end.
vef_value_callee() ->
	fun({_, V, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [Callee | _]} ->
				{V, Callee};
			error ->
				{V, none}
		end
	end.
vef_value_args() ->
	fun({_, V, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [_ | Args]} ->
				{V, Args};
			error ->
				{V, none}
		end
	end.
vef_poi_value_st() -> 
	fun({P, V, AI}) -> {P, V, dict:fetch(st, AI)} end.
vef_poi_value_ca() -> 
	fun({P, V, AI}) -> 
		case dict:find(ca, AI) of
			{ok, CA} ->
				{P, V, CA};
			error ->
				{P, V, none}
		end
	end.
vef_poi_value_callee() -> 
	fun({P, V, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [Callee | _]} ->
				{P, V, Callee};
			error ->
				{P, V, none}
		end
	end.
vef_poi_value_args() -> 
	fun({P, V, AI}) -> 
		case dict:find(ca, AI) of
			{ok, [_ | Args]} ->
				{P, V, Args};
			error ->
				{P, V, none}
		end
	end.
vef_full() ->
	fun({P, V, AI}) -> 
		case dict:find(ca, AI) of
			{ok, CA} ->
				{P, V, CA, dict:fetch(st,AI)}; 
			error ->
				{P, V, none, dict:fetch(st,AI)}
		end
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET INFO FROM THE TRACE ELEMENT %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_te_st({P, V, AI}) ->
	get_st(AI).

get_te_ca({P, V, AI}) ->
	get_ca(AI).

get_te_callee({P, V, AI}) ->
	get_callee(AI).

get_te_args({P, V, AI}) ->
	get_args(AI).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET INFO FORM THE ADDITIONAL INFORMATION %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_st(AI) ->
	dict:fetch(st, AI).

get_ca(AI) ->
	case dict:find(ca, AI) of
		{ok, CA} ->
			CA;
		error ->
			none
	end.

get_callee(AI) ->
	case dict:find(ca, AI) of
		{ok, CA} ->
			[Callee | _] = CA,
			Callee;
		error ->
			none
	end.

get_args(AI) ->
	case dict:find(ca, AI) of
		{ok, CA} ->
			[_ | Args] = CA,
			Args;
		error ->
			none
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BUILD UNEXPECTED BEHAVIOR REPORT MAPPING %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_ubrm() ->
	build_ubrm([]).

build_ubrm(L) ->
	NewL = [{default_error, fun default_report/3}, {length_error, fun length_report/3}, 
			{no_poi_relation, fun no_poi_report/3} | L],
	{Funs, Lists} = lists:foldr( 
					fun(E, {Fun, List}) ->
						{_Cod, Msg} = E,
						case is_list(Msg) of
							true -> 
								{Fun, [E | List]};
							false ->
								{[E | Fun], List}
						end
					end,
					{[],[]},
					NewL),
	ListFuns = lists:map(
						fun({Code, ShowFlags}) ->
							{Code, build_print_function(ShowFlags)}
						end,
						Lists),
	dict:from_list(Funs ++ ListFuns).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BUILD REPORT STRINGS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%
% build_print_functionv0(ShownFlags) ->
% 	fun({POE, VOE, AIOE}, {PNE, VNE, AINE}, His) ->
% 		{OldShownInfo, NewShownInfo} = 
% 			lists:foldr(
% 				 fun(val, {OldInfo, NewInfo}) -> 
% 				 		{OldValues, NewValues} = get_poi_history_info(POE, PNE, His),
% 						CompleteOldValues = lists:reverse([VOE | OldValues]), 
% 						CompleteNewValues = lists:reverse([VNE | NewValues]),
% 						{[value_string(CompleteOldValues) | OldInfo],[value_string(CompleteNewValues) | NewInfo]}; 
% 				    (st, {OldInfo, NewInfo}) -> {[stack_string(AIOE) | OldInfo], [stack_string(AINE) | NewInfo]};
% 				    (ca, {OldInfo, NewInfo}) -> {[call_string(AIOE) | OldInfo], [call_string(AINE) | NewInfo]};
% 				    (callee, {OldInfo, NewInfo}) -> {[callee_string(AIOE) | OldInfo], [callee_string(AINE) | NewInfo]};
% 				    (args, {OldInfo, NewInfo}) -> {[args_string(AIOE) | OldInfo], [args_string(AINE) | NewInfo]};
% 				    (_, Acc) -> Acc
% 				 end,
% 				 {[],[]},
% 	  			 ShownFlags),
% 		poi_string(POE) ++ lists:flatten(OldShownInfo) ++ "\n" ++
% 		poi_string(PNE) ++ lists:flatten(NewShownInfo)
% 	end.

build_print_function(ShownFlags) ->
	fun({POE, VOE, AIOE}, {PNE, VNE, AINE}, His) ->
			{OldShownInfo, NewShownInfo} = 
				lists:foldr(
					 fun(val, {OldInfo, NewInfo}) -> 
					 		{OldValues, NewValues} = get_poi_history_info(POE, PNE, His),
							CompleteOldValues = lists:reverse([VOE | OldValues]), 
							CompleteNewValues = lists:reverse([VNE | NewValues]),
							{[value_string(CompleteOldValues) | OldInfo],[value_string(CompleteNewValues) | NewInfo]}; 
					    (st, {OldInfo, NewInfo}) -> {[stack_string(AIOE) | OldInfo], [stack_string(AINE) | NewInfo]};
					    (ca, {OldInfo, NewInfo}) -> {[call_string(AIOE) | OldInfo], [call_string(AINE) | NewInfo]};
					    (callee, {OldInfo, NewInfo}) -> {[callee_string(AIOE) | OldInfo], [callee_string(AINE) | NewInfo]};
					    (args, {OldInfo, NewInfo}) -> {[args_string(AIOE) | OldInfo], [args_string(AINE) | NewInfo]};
					    (_, Acc) -> Acc
					 end,
					 {[],[]},
		  			 ShownFlags),
			poi_string(POE) ++ lists:flatten(OldShownInfo) ++ "\n" ++
			poi_string(PNE) ++ lists:flatten(NewShownInfo);
		(POs, {PNE, VNE, AINE}, His) ->
			{OldShownInfo, NewShownInfo} = 
			lists:foldr(
				 fun(val, {OldInfo, NewInfo}) -> 
				 		{OldValues, NewValues} = get_poi_history_info(POs, PNE, His),
				 		CompleteOldValues = lists:reverse(OldValues), 
						CompleteNewValues = lists:reverse([VNE | NewValues]),
						{[value_string(CompleteOldValues) | OldInfo],[value_string(CompleteNewValues) | NewInfo]}; 
				    (st, {OldInfo, NewInfo}) -> {[[] | OldInfo], [stack_string(AINE) | NewInfo]};
				    (ca, {OldInfo, NewInfo}) -> {[[] | OldInfo], [call_string(AINE) | NewInfo]};
				    (callee, {OldInfo, NewInfo}) -> {[[] | OldInfo], [callee_string(AINE) | NewInfo]};
				    (args, {OldInfo, NewInfo}) -> {[[] | OldInfo], [args_string(AINE) | NewInfo]};
				    (_, Acc) -> Acc
				 end,
				 {[],[]},
	  			 ShownFlags),
			poi_string(POs) ++ lists:flatten(OldShownInfo) ++ "\n" ++
			poi_string(PNE) ++ lists:flatten(NewShownInfo);
		({POE, VOE, AIOE}, PNs, His) ->
			{OldShownInfo, NewShownInfo} = 
				lists:foldr(
					 fun(val, {OldInfo, NewInfo}) -> 
					 		{OldValues, NewValues} = get_poi_history_info(POE, PNs, His),
							CompleteOldValues = lists:reverse([VOE | OldValues]), 
							CompleteNewValues = lists:reverse(NewValues),
							{[value_string(CompleteOldValues) | OldInfo],[value_string(CompleteNewValues) | NewInfo]}; 
					    (st, {OldInfo, NewInfo}) -> {[stack_string(AIOE) | OldInfo], [[] | NewInfo]};
					    (ca, {OldInfo, NewInfo}) -> {[call_string(AIOE) | OldInfo], [[] | NewInfo]};
					    (callee, {OldInfo, NewInfo}) -> {[callee_string(AIOE) | OldInfo], [[] | NewInfo]};
					    (args, {OldInfo, NewInfo}) -> {[args_string(AIOE) | OldInfo], [[] | NewInfo]};
					    (_, Acc) -> Acc
					 end,
					 {[],[]},
		  			 ShownFlags),
			poi_string(POE) ++ lists:flatten(OldShownInfo) ++ "\n" ++
			poi_string(PNs) ++ lists:flatten(NewShownInfo)
	end.


get_poi_history_info(POE, PNList, His) when is_list(PNList)->
	lists:foldl(fun({{OP, VO, _}, {NP, VN, _}}, {OValues, NValues}) ->
		case OP == POE andalso lists:member(NP,PNList) of
			true -> 
				{[VO | OValues], [VN | NValues]};
			false -> 
				{OValues, NValues}
		end
	end, 
	{[], []}, 
	His);
get_poi_history_info(POList, PNE, His) when is_list(POList) ->
	lists:foldl(fun({{OP, VO, _}, {NP, VN, _}}, {OValues, NValues}) ->
		case NP == PNE andalso lists:member(OP,POList) of
			true -> 
				{[VO | OValues], [VN | NValues]};
			false -> 
				{OValues, NValues}
		end
	end, 
	{[], []}, 
	His);
get_poi_history_info(POE, PNE, His) ->
	lists:foldl(fun({{OP, VO, _}, {NP, VN, _}}, {OValues, NValues}) ->
		case OP == POE andalso NP == PNE of
			true -> 
				{[VO | OValues], [VN | NValues]};
			false -> 
				{OValues, NValues}
		end
	end, 
	{[], []}, 
	His).

poi_string(POI) when is_list(POI) ->
	POIList = [poi_translation(P) || P <- POI],
	case length(POIList) of
		1 ->
			[P] = POIList, 
			format("POI: ~p\n",[P]);
		_ ->
			format("POIs: ~p\n",[POIList])
	end;
poi_string(POI) ->
	format("POI: ~p\n",[poi_translation(POI)]).

value_string(ValList) ->
	case is_string(ValList) of
		true ->
			format("  Trace:\n    ~s\n",[ValList]);
		partial ->
			format("  Trace:\n    ~p\n",[ValList]);
		false ->
			format("  Trace:\n    ~w\n",[ValList])
	end.

stack_string(AI) ->
	ST = dict:fetch(st,AI),
	FormattedST = 
		lists:map(fun({M,F,A,[_,L]}) ->
						{M,F,A,L};
					 (Tuple) ->
					 	Tuple
			  	  end,
			      ST),
	lists:foldl(fun(Elem,Acc) ->
					Acc ++ format("    ~p\n",[Elem])
				end,
				"  Stack\n",
				FormattedST).
	
call_string(AI) ->
	case dict:find(ca,AI) of
		{ok,[Callee | Args]} ->	% The Callee is a reference to a fun identifier. Think how to improve the clarity it provides
			"  Call POI Info:\n" ++
			callee_internal_string(Callee) ++
			format("    Args: [~s]\n",[args_internal_string(Args)]);
		error ->
			[]
	end.
callee_string(AI) ->
	case dict:find(ca, AI) of
		{ok, [Callee | _]} ->	% The Callee is a reference to a fun identifier. Think how to improve the clarity it provides
			"  Call POI Info:\n" ++
			callee_internal_string(Callee);
		error ->
			[]
	end.
args_string(AI) ->
	case dict:find(ca, AI) of
		{ok, [_ | Args]} ->
			"  Call POI Info:\n" ++
			format("    Args: [~s]\n",[args_internal_string(Args)]);
		error ->
			[]
	end.

callee_internal_string(Callee) ->
	% {_, ModName} = erlang:fun_info(Callee,module),
	% {_, FunName} = erlang:fun_info(Callee,name),
	% {_, Arity} = erlang:fun_info(Callee,arity),
	% CalleeString = 
	% 	case ModName of
	% 		erl_eval ->
	% 			format("fun ~p",[FunName]);
	% 		_ ->
	% 			format("~p:~p/~p",[ModName, FunName, Arity])
	% 	end,
	% format("    Callee: ~s\n",[CalleeString]).
	format("    Callee: ~p\n",[Callee]).

args_internal_string(Args) ->
  	ArgList = lists:map(fun(Arg) ->
              case is_string(Arg) of
                true ->
                    format("\"~s\"", [Arg]);
                _ when is_list(Arg) ->
                    format("[~s]", [args_internal_string(Arg)]);
                _ ->
                	format("~w", [Arg])
              end
            end,
            Args),
  	FinalArgs = lists:foldl(
            fun(E,[]) ->
              E;
              (E,S) ->
                S ++ "," ++ E
            end,
            [],
            ArgList), 
  	format("~s",[FinalArgs]).

%%%%%%%%%%%%%%%%%%%%
%% DEFAULT REPORT %%
%%%%%%%%%%%%%%%%%%%%
default_report({POE, VOE, _}, {PNE, VNE, _}, His) ->
	{OldValues, NewValues} = get_poi_history_info(POE, PNE, His),
	CompleteOldValues = lists:reverse([VOE | OldValues]), 
	CompleteNewValues = lists:reverse([VNE | NewValues]), 
	poi_string(POE) ++ value_string(CompleteOldValues) ++
	poi_string(PNE) ++ value_string(CompleteNewValues).

length_report({POE, VOE, _}, PNs, His) when is_list(PNs) ->
	{OldValues, NewValues} = get_poi_history_info(POE, PNs, His),
	CompleteOldValues = lists:reverse([VOE | OldValues]), 
	poi_string(POE) ++ value_string(CompleteOldValues) ++
	poi_string(PNs) ++ value_string(NewValues);
length_report(POs, {PNE, VNE, _}, His) when is_list(POs) ->
	{OldValues, NewValues} = get_poi_history_info(POs, PNE, His),
	CompleteNewValues = lists:reverse([VNE | NewValues]), 
	poi_string(POs) ++ value_string(OldValues) ++
	poi_string(PNE) ++ value_string(CompleteNewValues).

no_poi_report({POE, _, _}, {PNE, _, _}, _) ->
	format("Unrelated POIs found:\n"
	"+ ~p\n"
	"+ ~p\n",[POE,PNE]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% GENERAL COMPARISON FUNCTION %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cf_general() ->
	cf_general(#config{}).

cf_general(Conf = #config{tecf = TECF, ubrm = UBRM}) ->
	NTECF	= 
			case TECF of 
				undefined ->
					build_tecf();
				_ ->
					TECF
			end,
	NUBRM =
		case UBRM of
			undefined ->
				build_ubrm();
			_ ->
				build_ubrm(UBRM)
		end,
	NConf = Conf#config{tecf = NTECF, ubrm = NUBRM},
	fun(TO, TN, PoiRel, InternalIdDict) ->
		cf_general(TO, TN, NConf, PoiRel, InternalIdDict)
	end.

cf_general(TO, TN, Conf, PoiRel, InternalIdDict) ->  
	cf_general(TO, TN, Conf, PoiRel, InternalIdDict, []).

cf_general([], [], _, _, _, _) -> 
	true;
cf_general([TOE | _], [], Conf = #config{ubrm = UBRM}, PoiRel, InternalIdDict, His) -> 
	case His of
		[] ->
			{
				false, 
				second_trace_empty, 
				fun() -> 
					special_fetch(second_trace_empty, UBRM, {old, TOE}, lists:reverse(His), PoiRel, InternalIdDict) 
				end
			};
		_ ->
			{
				false, 
				first_trace_longer, 
				fun() -> 
					special_fetch(first_trace_longer, UBRM, {old, TOE}, lists:reverse(His), PoiRel, InternalIdDict) 
				end
			}
	end;
cf_general([], [TNE | _], Conf = #config{ubrm = UBRM}, PoiRel, InternalIdDict, His) -> 
	case His of
		[] ->
			{
				false, 
				first_trace_empty, 
				fun() -> 
					special_fetch(first_trace_empty, UBRM, {new, TNE}, lists:reverse(His), PoiRel, InternalIdDict) 
				end
			};
		_ ->
			{
				false, 
				second_trace_longer, 
				fun() -> 
					special_fetch(second_trace_longer, UBRM, {new, TNE}, lists:reverse(His), PoiRel, InternalIdDict) 
				end
			}
	end;
cf_general([TOE | TO], [TNE | TN], Conf = #config{tecf = TECF, ubrm = UBRM}, PoiRel, InternalIdDict, His) -> 
	case related_pois(TOE, TNE, PoiRel) of
		true ->
			case TECF(TOE, TNE) of
				true ->
					cf_general(TO, TN, Conf, PoiRel, InternalIdDict, [{TOE , TNE} | His]);
				UBT -> {
					false, 
					UBT, 
					fun() ->
						my_fetch(UBT, UBRM, TOE, TNE, lists:reverse(His), InternalIdDict)
					end
					} 
			end;
		UBT ->
			{false, UBT, 
				fun() ->
					my_fetch(UBT, UBRM, TOE, TNE, lists:reverse(His), InternalIdDict)
				end
			}
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

special_fetch(UBT, UBRM, {old, {PO, VO, AIO}}, His, PoiRel, InternalIdDict) ->
	PNs = get_related_pois(PO, old, PoiRel),
	NewTOE = {dict:fetch(PO, InternalIdDict), VO, AIO}, 
	NewPNs = [dict:fetch(P, InternalIdDict) || P <- PNs],
	NewHis = lists:map(fun({{PO, VO, AIO}, {PN, VN, AIN}}) ->
						 {{dict:fetch(PO, InternalIdDict), VO, AIO}, {dict:fetch(PN, InternalIdDict), VN, AIN}}
					   end, 
					   His),
	(dict:fetch(length_error, UBRM))(NewTOE, NewPNs, NewHis);
special_fetch(UBT, UBRM, {new, {PN, VN, AIN}}, His, PoiRel, InternalIdDict) ->
	POs = get_related_pois(PN, new, PoiRel),
	NewTNE = {dict:fetch(PN, InternalIdDict), VN, AIN}, 
	NewPOs = [dict:fetch(P, InternalIdDict) || P <- POs],
	NewHis = lists:map(fun({{PO, VO, AIO}, {PN, VN, AIN}}) ->
						 {{dict:fetch(PO, InternalIdDict), VO, AIO}, {dict:fetch(PN, InternalIdDict), VN, AIN}}
					   end, 
					   His),
	(dict:fetch(length_error, UBRM))(NewPOs, NewTNE, NewHis).

undo_id_transformation({POld, VOld, AIOld}, {PNew, VNew, AINew}, His, IdDict) ->
	{
		{dict:fetch(POld, IdDict), VOld, AIOld}, 
		{dict:fetch(PNew, IdDict), VNew, AINew}, 
		lists:map(fun({{PO, VO, AIO}, {PN, VN, AIN}}) ->
					{{dict:fetch(PO, IdDict), VO, AIO}, {dict:fetch(PN, IdDict), VN, AIN}}
				  end, 
				  His)
	}.

get_related_pois(P, Flag, Rels) ->
	case Flag of
		old ->
			lists:filtermap(
						 fun({OP, NP}) ->
						 	case OP == P of
						 		true ->
						 			{true, NP};
						 		false ->
						 			false
						 	end
						 end,
						 Rels);
		new ->
			lists:filtermap(
						 fun({OP, NP}) ->
						 	case NP == P of
						 		true ->
						 			{true, OP};
						 		false ->
						 			false
						 	end
						 end,
						 Rels)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% INDEPENDENT COMPARISON FUNCTION %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cf_independent() ->
	cf_independent(#config{}).

cf_independent(Conf = #config{tecf=TECF, ubrm=UBRM}) ->
	fun(TO, TN, PoiRel, InternalIdDict) ->
		NTECF	= 
			case TECF of 
				undefined ->
					build_tecf();
				_ ->
					TECF
			end,
		NUBRM =
			case UBRM of
				undefined ->
					build_ubrm();
				_ ->
					build_ubrm(UBRM)
			end,
		NConf = Conf#config{tecf = NTECF, ubrm = NUBRM},
		cf_independent(TO, TN, NConf, PoiRel, InternalIdDict)
	end.

% WARNING: This function returns a list: [true | {false, UBT, {P1, P2, His}}]. This list must be treated afterwards
cf_independent(TO, TN, Conf, PoiRel, InternalIdDict) -> 
	TraceList = trace_division(TO, TN, PoiRel), 
	lists:map(fun({TOIndependent, TNIndependent}) ->
				cf_general(TOIndependent, TNIndependent, Conf, PoiRel, InternalIdDict, [])
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
equals() ->
	fun secer_api:equals/2.
equals(Elem1, Elem2) ->
	case  Elem1 == Elem2 of
	   	true -> true;
	   	false -> different_value
	end.

different() ->
	fun secer_api:different/2.
different(Elem1, Elem2) ->
	case  Elem1 /= Elem2 of
	   	true -> true;
	   	false -> equal_value
	end.

lower_than() ->
	fun secer_api:lower_than/2.
lower_than(Elem1, Elem2) -> 
	case Elem1 > Elem2 of
		true->
			true;
		false when Elem1 == Elem2 ->
			equal_value;
		_ ->
			greater_value
	end.

greater_than() ->
	fun secer_api:greater_than/2.
greater_than(Elem1, Elem2) -> 
	case Elem1 < Elem2 of
		true->
			true;
		false when Elem1 == Elem2 ->
			equal_value;
		_ ->
			lower_value
	end.

%%%%%%%%%%%%%%%%%%%%%%
%% COMMON FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%
poi_translation(Poi) when is_list(Poi) ->
	[poi_translation(P) || P <- Poi];
poi_translation(Poi) ->
	case Poi of
		{F, L, application, O} ->
			{F, L, call, O};
		{F, L, try_expr, O} ->
			{F, L, 'try', O};
		{F, L, if_expr, O} ->
			{F, L, 'if', O};
		{F, L, case_expr, O} ->
			{F, L, 'case', O};
		{F, L, list_comp, O} ->
			{F, L, lc, O};
		_ ->
			Poi
	end.

format(Str,Args) ->
	lists:flatten(io_lib:format(Str,Args)).

is_string(List) when is_list(List) -> 
  case lists:all(fun is_print/1, List) of
    true ->
      true;
    false ->
      case lists:all(fun is_list/1, List) of
        true ->
          is_internal_string(List);
        false ->
          false
      end
  end;
is_string(_) -> false.


is_internal_string(List) ->
  catch lists:foldl(
          fun(Elem,_) -> 
            case is_print(Elem) of
              true ->
                partial;
              false when is_list(Elem) ->
                is_internal_string(Elem);
              false ->
                throw(false)
            end
          end,
        true,
        List).

is_print(X) when X >= 32, X < 127 -> true;
is_print(_) -> false.

printer(Node) -> io:format("~p\n", [Node]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: REVIEW THE REST OF THE FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comp_perf(Thresh) -> 
% 	fun(TO, TN, PoiRel) -> 
% 		comp_perf(TO, TN, PoiRel, Thresh) 
% 	end.

% comp_perf(TO, TN, _, Thresh) ->
%     ZippedList = lists:zip(TO, TN), 
%     lists:foldl(
%         fun
%             (_, {false, Msg, P1, P2}) ->
%                 {false, Msg, P1, P2};
%             ({{_, VO}, {_, VN}}, _) when (VO + Thresh) >= VN ->
%                 true;
%             ({{P1, _}, {P2, _}}, _) ->
%                 {false, "Slower Calculation", P1, P2}
%         end, 
%         true, 
%         ZippedList).

% show() ->
% 	fun secer_cfuns:show/3.
% show(TO, TN, _) -> 
% 	FunShowTrace = 
%  		fun(T) ->
%  			lists:map(
%  				fun({POI, Value}) ->
%  					io:format("POI: ~p\nValue: ~p\n", [POI, Value])
%  				end, 
%  			T)
% 		end, 
% 	io:format("Trace old version:\n"), 
% 	FunShowTrace(TO), 
% 	io:format("Trace new version:\n"), 
% 	FunShowTrace(TN), 
% 	true.     

% lists_comp_perf(Threshold) -> 
% 	fun (TO, TN, PoiRel) -> 
% 		lists_comp_perf(TO, TN, PoiRel, Threshold) 
% 	end.
% lists_comp_perf(TO, TN, PoiRel, Threshold) -> 
%   FunWhenTrue = 
%   	fun(_, _, _) ->
%   		ok
%   	end, 
%   lists_comp_perf_common(TO, TN, PoiRel, Threshold, FunWhenTrue).

% io_lists_comp_perf(TO, TN, PoiRel, Threshold) -> 
%   FunWhenTrue = 
%   	fun(I, VO, VN) ->
%   		io:format(
%   			"Faster Calculation: Length: ~p -> ~p vs ~p ms.\n", 
%   			[length(I), VO, VN])
%   	end, 
%   lists_comp_perf_common(TO, TN, PoiRel, Threshold, FunWhenTrue).

% lists_comp_perf_common(TO, TN, _, Threshold, FunWhenTrue) -> 
%   ZippedList = lists:zip(TO, TN), 
%   lists:foldl(
%     fun
%       (_, {false, Msg, P1, P2}) ->
%         {false, Msg, P1, P2};
%       ({{_, {_, VO}}, {_, {_, VN}}}, _) when abs(VO - VN) =< Threshold -> 
%          true;
%       ({{_, {I, VO}}, {_, {I, VN}}}, _) when VO >= VN ->
%       	 FunWhenTrue(I, VO, VN), 
%          true;
%       ({{PO, {I, VO}}, {PN, {I, VN}}}, _) ->
%          {
%             false, 
%             lists:flatten(
%               io_lib:format(
%                 "Slower Calculation: Length: ~p -> ~p vs ~p µs.", 
%                 [length(I), VO, VN])), 
%             PO, PN
%           }
%     end, 
%     true, 
%     ZippedList).


