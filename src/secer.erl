-module(secer).
-export([run/5,run/6,run/7]).
% PoisRels::[{POIOld,POINew}] 
% CompareFun::fun cf/2
% Fun::"funName/Arity" (String)
% Timeout::integer

run(PoisRels,ExecFun,Timeout,CMode,CompareFun) -> 
	try 
		case whereis(secer) of
		    undefined ->
		        register(secer, self());
		    _ ->
		        already_started
		end,

		register(input_manager,spawn(secer_im_server,init,[])),
		register(input_gen,spawn(secer_input_gen,main,[PoisRels,ExecFun,Timeout,CMode,CompareFun])),
%		AUX2 = (catch register(input_gen,spawn(secer_input_gen,main,[PoisRels,ExecFun,Timeout,CMode,CompareFun]))),
%		printer("AUX2"),
%		printer(AUX2),

		{FunName,Arity} = get_function_name(ExecFun),

		receive
			die -> 
				exit(0);
			continue ->
				%printer("CONTINUA"),
				ok
		after Timeout * 1000 ->
				ok
		end,
		input_manager ! {get_results,self()},

		receive
			{Empty,Valued,Same,Different,_Cvg,IdPoiDict,Timeouted,TraceDict} -> 
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
										trace_organization(V,K,Dic,[])
								end
							end,
							dict:new(),
							Different),
						CountList = dict:to_list(CountDic),
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
%printer(CountList),
						io:format("    POIs comparison:\n"),
						ErrorList = 
							[ 
								[begin
%printer(NumError),
									case NumError of
										0 ->
											io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
											io:format("\t\t Unexpected trace value => ~w Errors\n",[C]),
											InputString = lists:flatten(io_lib:format("~w", [EI])),
											FinalInput = string:substr(InputString,2,length(InputString)-2),
											io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput]);
										1 ->
											io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
											io:format("\t\t The first trace is longer => ~w Errors\n",[C]),
											InputString = lists:flatten(io_lib:format("~w", [EI])),
											FinalInput = string:substr(InputString,2,length(InputString)-2),
											io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput]);
										2 ->
											io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
											io:format("\t\t The second trace is longer => ~w Errors\n",[C]),
											InputString = lists:flatten(io_lib:format("~w", [EI])),
											FinalInput = string:substr(InputString,2,length(InputString)-2),
											io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput]);
										3 ->
											io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
											io:format("\t\t The second trace is empty => ~w Errors\n",[C]),
											InputString = lists:flatten(io_lib:format("~w", [EI])),
											FinalInput = string:substr(InputString,2,length(InputString)-2),
											io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput]);
										4 ->
											io:format("\t+ ~p\n",[{poi_translation(POI1),poi_translation(POI2)}]),
											io:format("\t\t The first trace is empty => ~w Errors\n",[C]),
											InputString = lists:flatten(io_lib:format("~w", [EI])),
											FinalInput = string:substr(InputString,2,length(InputString)-2),
											io:format("\t\t Example call: ~s(~s)\n",[FunName,FinalInput])
									end,
									EI
								 end || {C,EI,NumError} <- Errors]
							|| {{POI1,POI2},Errors} <- CountList],
						lists:map(
							fun(EL) ->
								lists:map(
									fun(I) -> 
										{ok,Val} = dict:find(I,Different),
										print_detected_error(FunName,IdPoiDict,I,Val)
									end,
									EL)
							end,
							ErrorList)
				end;

				% {ok,Fd} = file:open("./tmp/Results.txt",[write]),
				% io:format(Fd,"~w.\n~w.",[Same,Different]);


%%%%%%%%% ESCRIBIR Inputs.txt en ./tmp al ejecutar %%%%%%%%%
% {ok,FdI} = file:open("./tmp/Inputs.txt",[write]),

% io:format(FdI,"*** NORMAL EXECUTIONS ***\n",[]), 
% dict:map(
% 	fun(K,_) -> 
% 		InputString0 = lists:flatten(io_lib:format("~w", [K])),
% 		FinalInput0 = string:substr(InputString0,2,length(InputString0)-2),
% 		io:format(FdI,"~s(~s)\n",[FunName,FinalInput0]) 
% 	end,
% 	Same),
% io:format(FdI,"*** ERROR EXECUTIONS ***\n",[]),
% dict:map(
% 	fun(K,V) -> 
% 		InputString0 = lists:flatten(io_lib:format("~w", [K])),
% 		FinalInput0 = string:substr(InputString0,2,length(InputString0)-2),
% 		io:format(FdI,"~s(~s)\n",[FunName,FinalInput0]) 
% 	end,
% 	Different),
% io:format(FdI,"*** TIMEOUTED EXECUTIONS ***\n",[]), 
% dict:map(
% 	fun(K,_) -> 
% 		InputString0 = lists:flatten(io_lib:format("~w", [K])),
% 		FinalInput0 = string:substr(InputString0,2,length(InputString0)-2),
% 		io:format(FdI,"~s(~s)\n",[FunName,FinalInput0]) 
% 	end,
% 	Timeouted);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
		E:R -> 
			%printer(erroraco),
			errorCatch
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
		file:delete(filename:basename(FileNew,".erl")++".beam"),
		clean_tmp()
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
	{T2,_} = case is_list(Poi2) of
		true ->
			lists:foldl(
				fun({Id,T},{Acc,PL}) ->
					case dict:find(Id,IdPoiDict) of
						{ok,P} -> 
							case lists:member(P,PL) of 
								true ->
									{[T|Acc],PL};
								_ ->
									{Acc,PL}
							end;
						_ ->
							{Acc,PL}
					end
				end,
				{[],Poi2},
				Poi2List);
		false ->
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
				Poi2List) 
	end,

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

trace_organization(V,K,Dic,L) ->
	{T1,T2,Msg,P1,P2} = V,
	case Msg of
		"The length of both traces differs" ->
			% printer({T1,T2}),
			% io:get_line("STOP"),
			case length(T1) > length(T2) of
				true ->
					case L of
						[] ->
							dict:store({P1,P2},[{1,K,1}],Dic);
						_ ->
							trace_type_organization(P1,P2,K,L,Dic,1)
					end;
				false ->
					case L of
						[] ->
							dict:store({P1,P2},[{1,K,2}],Dic);
						_ ->
							trace_type_organization(P1,P2,K,L,Dic,2)
					end
			end;
		"The first trace is empty" ->
			case L of
				[] ->
					dict:store({P1,P2},[{1,K,4}],Dic);
				_ ->
					trace_type_organization(P1,P2,K,L,Dic,4)
			end;
		"The second trace is empty" ->
			case L of
				[] ->
					dict:store({P1,P2},[{1,K,3}],Dic);
				_ ->
					trace_type_organization(P1,P2,K,L,Dic,3)
			end;
		_ ->
			case L of
				[] ->
					dict:store({P1,P2},[{1,K,0}],Dic);
				_ ->
					trace_type_organization(P1,P2,K,L,Dic,0)
			end
	end.

trace_type_organization(P1,P2,K,L,Dic,ErrorId) ->
	Exists = lists:any(fun(E) -> ErrorId == element(3,E) end,L),
	NewL = case Exists of
		true ->
			lists:map(
				fun(E) ->
					case E of
						{Cont,I,ErrorId} ->
							{Cont+1,I,ErrorId};
						_ ->
							E
					end
				end,
				L);
		false ->
			[{1,K,ErrorId}|L]
	end,
	dict:store({P1,P2},NewL,Dic).
% trace_organization(V,K,Dic,{{C1,I1,L1},{C2,I2,L2}}) ->
% 	{T1,T2,Msg,P1,P2} = V,
	
% 	case Msg of
% 		"The length of both traces differs" ->
% 			case length(T1) > length(T2) of
% 				true ->
% 					ExInput = 
% 						case I1 of
% 							null ->
% 								K;
% 							_ ->
% 								I1
% 						end,
% 					dict:store({P1,P2},{{C1+1,ExInput,1},{C2,I2,L2}},Dic);
% 				false ->
% 					ExInput = 
% 						case I2 of
% 							null ->
% 								K;
% 							_ ->
% 								I2
% 						end,
% 					dict:store({P1,P2},{{C1,I1,L1},{C2+1,ExInput,2}},Dic)
% 			end;
% 		_ ->
% 			ExInput = 
% 				case I1 of
% 					null ->
% 						K;
% 					_ ->
% 						I1
% 				end,
% 			dict:store({P1,P2},{{C1+1,ExInput,0},{C2,I2,L2}},Dic)
% 	end.

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SILENT EXECUTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(PoisRels,ExecFun,Timeout,CMode,CompareFun,silent) ->
	{ok,FdR} = file:open("./tmp/repeat.txt",[append]),
	{S,D} = run_silent(PoisRels,ExecFun,Timeout,CMode,CompareFun,mutation),
	%io:format("~p ~p\n",[S,D]),
 	%io:format(FdR,"Total:~p Differents:~p Differents(%):~p\n",[S+D,D,trunc((D/(S+D))*10000)/100]).
 	io:format(FdR,"~p ~p\n",[S+D,D]).

run(PoisRels,ExecFun,Timeout,CMode,CompareFun,silent,random) ->
	{ok,FdR} = file:open("./tmp/repeat.txt",[append]),
	{S,D} = run_silent(PoisRels,ExecFun,Timeout,CMode,CompareFun,random),
	%io:format("~p ~p\n",[S,D]),
 	%io:format(FdR,"Total:~p Differents:~p Differents(%):~p\n",[S+D,D,trunc((D/(S+D))*10000)/100]).
 	io:format(FdR,"~p ~p\n",[S+D,D]).


run_silent(PoisRels,ExecFun,Timeout,CMode,CompareFun,GenMode) -> 
	try 
		case whereis(secer) of
		    undefined ->
		        register(secer, self());
		    _ ->
		        already_started
		end,

		register(input_manager,spawn(secer_im_server,init,[])),
		case GenMode of
			random ->
				register(input_gen,spawn(secer_input_gen,main_random,[PoisRels,ExecFun,Timeout,CMode,CompareFun]));
			mutation ->
				register(input_gen,spawn(secer_input_gen,main,[PoisRels,ExecFun,Timeout,CMode,CompareFun]))
		end,
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
			{Empty,Valued,Same,Different,_Cvg,IdPoiDict,Timeouted,TraceDict} -> 
				{dict:size(Same),dict:size(Different)};
			_ ->
				printer(error),
				error
		end
	catch 
		E:R -> 
			errorCatch
	after
		case {whereis(input_gen),whereis(input_manager)} of
			{undefined,undefined} ->
				ok;
			{undefined,Pid} ->
				%unregister(input_manager),
				timer:exit_after(0,Pid,kill);
			{Pid,undefined} ->
				%unregister(input_gen),
				timer:exit_after(0,Pid,kill);
			{Pid1,Pid2} ->
				%unregister(input_manager),
				%unregister(input_gen),
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
		file:delete(filename:basename(FileNew,".erl")++".beam"),
		clean_tmp()
	end.

clean_tmp() ->
	{ok,Files} = file:list_dir("./tmp/"),
	[ begin
		case filename:extension(File) of
			".beam" ->
				file:delete("./tmp/"++File);
			_ ->
				ok
		end
	  end || File <- Files].


