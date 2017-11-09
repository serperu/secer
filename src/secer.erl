-module(secer).
-export([run/5]).
% PoisRels::[{POIOld,POINew}] 
% CompareFun::fun cf/2
% Fun::"funName/Arity" (String)
% Timeout::integer

run(PoisRels,ExecFun,Timeout,CMode,CompareFun) -> 
	try 
		register(secer,self()),
		register(input_gen,spawn(secer_input_gen,main,[PoisRels,ExecFun,Timeout,CMode,CompareFun])),
		register(input_manager,spawn(secer_im_server,init,[])),

		{FunName,Arity} = get_function_name(ExecFun),

		receive
			die -> 
				exit(0);
			continue ->
				ok
		after Timeout * 1000 ->
				ok
		end,

		input_manager ! {get_results,self()},

		receive
			{Empty,Valued,Same,Different,_Cvg,IdPoiDict} -> 

				case dict:size(Different) of
					0 ->
						io:format("\n"),
						io:format("Function: ~s/~s\n",[FunName,Arity]),
						io:format("~s\n",["----------------------------"]),
						io:format("Generated test cases: ~p\n",[dict:size(Same)]),
						io:format("Both versions of the program generate identical traces for the defined points of interest\n"),
						io:format("~s\n",["----------------------------"]);
					X ->
						SameTests = dict:size(Same),
						io:format("\n"),
						io:format("Function: ~s/~s\n",[FunName,Arity]),
						io:format("~s\n",["----------------------------"]),
						io:format("Generated test cases: ~p\n",[SameTests+X]),
						Percentage = trunc((X/(SameTests+X))*10000)/100,

						CountDic = dict:fold(
							fun(K,V,Dic) ->
								% Hecho para 1 Relacion de POIs, hay que cambiarlo cuando hayan varios,
								% miro la traza entera, pero no solo los relativos a la relacion {P1,P2}
								{_,_,_,P1,P2} = V,
								case dict:find({P1,P2},Dic) of
									{ok,Value} ->
										trace_organization(V,K,Dic,Value);
									error ->
										trace_organization(V,K,Dic,{{0,null,-1},{0,null,-1}})
								end
							end,
							dict:new(),
							Different),
						CountList = dict:to_list(CountDic),
						%printer(CountList),
						io:format("Mismatching test cases: ~p (~p%)\n",[X,Percentage]),
						
						%io:format("All mismatching results were saved at: ./results/~s.txt\n",[FunName++"_"++Arity]),

						%{ErrorInput,Report} = (catch dict:map(fun(K,V) -> throw({K,V}) end, Different)),
						%{Poi1List,Poi2List,ErrorMsg,Poi1,Poi2} = Report,
						% Aqui debemos tener cuidado porque puede llegar una lista en Poi1 
						% o Poi2 con los POIs relacionados con el otro al ser un error diferent_length
						% case Poi1 of
						% 	"User Defined" ->
						% 		ok;
						% 	_ ->
						io:format("    POIs comparison:\n"),
						ErrorList = 
							[begin
								case {L1,L2} of
									{0,_} ->
										io:format("\t+ ~p => ~w Errors\n",[{poi_translation(POI1),poi_translation(POI2)},C1]),
										InputString = lists:flatten(io_lib:format("~w", [EI1])),
										FinalInput = string:substr(InputString,2,length(InputString)-2),
										io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput]);
									{1,2} ->
										io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
										io:format("\t\t The first trace is longer => ~w Errors\n",[C1]),
										InputString1 = lists:flatten(io_lib:format("~w", [EI1])),
										FinalInput1 = string:substr(InputString1,2,length(InputString1)-2),
										io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput1]),
										io:format("\t\t The second trace is longer => ~w Errors\n",[C2]),
										InputString2 = lists:flatten(io_lib:format("~w", [EI2])),
										FinalInput2 = string:substr(InputString2,2,length(InputString2)-2),
										io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput2]);
									{-1,2} ->
										io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
										io:format("\t\t The second trace is longer => ~w Errors\n",[C2]),
										InputString2 = lists:flatten(io_lib:format("~w", [EI2])),
										FinalInput2 = string:substr(InputString2,2,length(InputString2)-2),
										io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput2]);
									{1,-1} ->
										io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
										io:format("\t\t The first trace is longer => ~w Errors\n",[C1]),
										InputString1 = lists:flatten(io_lib:format("~w", [EI1])),
										FinalInput1 = string:substr(InputString1,2,length(InputString1)-2),
										io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput1])
								end,
								{EI1,EI2}
							 end || {{POI1,POI2},{{C1,EI1,L1},{C2,EI2,L2}}} <- CountList],
						%end,
						lists:map(
							fun({I1,I2}) ->
								case I1 of
									null ->
										ok;
									_ ->
										{ok,Val1} = dict:find(I1,Different),
										print_detected_error(FunName,IdPoiDict,I1,Val1)
								end,
								case I2 of
									null ->
										ok;
									_ ->
										{ok,Val2} = dict:find(I2,Different),
										print_detected_error(FunName,IdPoiDict,I2,Val2)
								end
							end,
							ErrorList)
				end;

				% {ok,Fd} = file:open("./tmp/Results.txt",[write]),
				% io:format(Fd,"~w.\n~w.",[Same,Different]);

				% ReadeableFileName = "./results/"++FunName++"_"++Arity++".txt",
				% {ok,Fd2} = file:open(ReadeableFileName,[write]),
				% Mod1 = filename:basename(File1,".erl"),
				% Mod2 = filename:basename(File2,".erl"),
				% io:format(Fd2,"****************************************\n",[]),
				% io:format(Fd2,"EXECUTION ----> {~s_Trace, ~s_Trace}\n",[Mod1,Mod2]),
				% io:format(Fd2,"****************************************\n",[]),
				% io:format(Fd2,"~s Interest Point: ~p\n",[Mod1,Id1]),
				% io:format(Fd2,"~s Interest Point: ~p\n",[Mod2,Id2]),
				% io:format(Fd2,"****************************************\n",[]),
				% %io:format(Fd2,"DIFFERENT TRACES\n",[]),
				% dict:map(fun(K,V) -> 
				% 		InputString0 = lists:flatten(io_lib:format("~w", [K])),
				% 		FinalInput0 = string:substr(InputString0,2,length(InputString0)-2),
				% 		io:format(Fd2,"~s(~s) ----> ~w\n",[FunName,FinalInput0,V]) end,Different);
				% % io:format(Fd2,"*** EXECUTION ----> [COMMON TRACE] ***\n",[]),
				% % io:format(Fd2,"COMMON TRACES TRACES\n",[]),
				% % dict:map(fun(K,V) -> 
				% % 		InputString0 = lists:flatten(io_lib:format("~w", [K])),
				% % 		FinalInput0 = string:substr(InputString0,2,length(InputString0)-2),
				% % 		io:format(Fd2,"~s(~s) ----> ~w\n",[FunName,FinalInput0,V]) end,Same);
				% % dict:map(fun(K,V) -> io:format(Fd,"~p\n",[V]) end,Valued),
				
				% % printer("Coverage"),
				% % printer(Cvg),


				% Save results in projects/default/default.txt file
			_ ->
				printer(error),
				error
		end
	catch 
		_:_ -> errorCatch
	after
		case {whereis(input_gen),whereis(input_manager)} of
			{undefined,undefined} ->
				ok;
			{undefined,Pid} ->
				unregister(input_manager),
				timer:exit_after(0,Pid,kill);
			{Pid,undefined} ->
				unregister(input_gen),
				timer:exit_after(0,Pid,kill);
			{Pid1,Pid2} ->
				unregister(input_manager),
				unregister(input_gen),
				timer:exit_after(0,Pid1,kill),
				timer:exit_after(0,Pid2,kill)
		end,
		[{Old,New}|_] = PoisRels,
		
		FileOld = atom_to_list(element(1,Old)),
		FileNew = atom_to_list(element(1,New)),

		TmpM1 = "./tmp/"++filename:basename(FileOld,".erl")++"Tmp",
		TmpM2 = "./tmp/"++filename:basename(FileNew,".erl")++"Tmp",

		file:delete(TmpM1++".erl"),
		file:delete(TmpM2++".erl"),
		file:delete(TmpM1++".beam"),
		file:delete(TmpM2++".beam"),

		file:delete("./tmp/"++FileOld),
		file:delete("./tmp/"++FileNew),

		file:delete(filename:basename(FileOld,".erl")++".beam"),
		file:delete(filename:basename(FileNew,".erl")++".beam")
	end.

print_detected_error(FunName,IdPoiDict,ErrorInput,Val) ->
	{Poi1List,Poi2List,ErrorMsg,Poi1,Poi2} = Val,
	{T1,_} = 
		lists:foldl(
			fun({Id,T},{Acc,P}) ->
				case dict:find(Id,IdPoiDict) of
					{ok,P} -> 
						{[T|Acc],P};
					_ ->
						{Acc,P}
				end
			end,
			{[],Poi1},
			Poi1List),
	{T2,_} = 
		lists:foldl(
			fun({Id,T},{Acc,P}) ->
				case dict:find(Id,IdPoiDict) of
					{ok,P} -> 
						{[T|Acc],P};
					_ ->
						{Acc,P}
				end
			end,
			{[],Poi2},
			Poi2List), 
	OriginalPoi1 = poi_translation(Poi1),
	OriginalPoi2 = poi_translation(Poi2),

	io:format("~s\n",["------ Detected Error ------"]),
	InputString = lists:flatten(io_lib:format("~w", [ErrorInput])),
	FinalInput = string:substr(InputString,2,length(InputString)-2),
	io:format("Call: ~s(~s)\n",[FunName,FinalInput]),
	io:format("Error Type: ~s\n",[ErrorMsg]),
	case Poi1 of
		"User Defined" ->
			ok;
		_ ->
			io:format("POI: (~p) trace:\n\t ~w\n",[OriginalPoi1,lists:reverse(T1)]),
			io:format("POI: (~p) trace:\n\t ~w\n",[OriginalPoi2,lists:reverse(T2)])
	end,
	io:format("~s\n",["----------------------------"]).

trace_organization(V,K,Dic,{{C1,I1,L1},{C2,I2,L2}}) ->
	{T1,T2,Msg,P1,P2} = V,
	
	%printer(ExInput),
	case Msg of
		"The length of both traces differs" ->
			case length(T1) > length(T2) of
				true ->
					ExInput = 
						case I1 of
							null ->
								K;
							_ ->
								I1
						end,
					dict:store({P1,P2},{{C1+1,ExInput,1},{C2,I2,L2}},Dic);
				false ->
					ExInput = 
						case I2 of
							null ->
								K;
							_ ->
								I2
						end,
					dict:store({P1,P2},{{C1,I1,L1},{C2+1,ExInput,2}},Dic)
			end;
		_ ->
			ExInput = 
				case I1 of
					null ->
						K;
					_ ->
						I1
				end,
			dict:store({P1,P2},{{C1+1,ExInput,0},{C2,I2,L2}},Dic)
	end.

poi_translation(Poi) when is_list(Poi) ->
	[poi_translation(P) || P <- Poi];
poi_translation(Poi) ->
	case Poi of
		{F,L,application,O} ->
			{F,L,call,O};
		{F,L,try_expr,O} ->
			{F,L,'try',O};
		{F,L,if_expr,O} ->
			{F,L,'if',O};
		{F,L,case_expr,O} ->
			{F,L,'case',O};
		_ ->
			Poi
	end.

get_function_name(FunArity) ->
	Tokens = string:tokens(FunArity,"/"),
	[Name,Arity] = Tokens,
	{Name,Arity}.

printer(X) -> io:format("~p\n",[X]).