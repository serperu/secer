-module(secer).
-export([run/1]).
run([File1,Line1,Var1,Oc1,File2,Line2,Var2,Oc2,Fun,Time]) ->
	try 
		TimeOut = list_to_integer(Time),
		register(secer,self()),
		register(input_gen,spawn(secer_input_gen,main,[File1,Line1,Var1,Oc1,File2,Line2,Var2,Oc2,Fun,TimeOut])),
		register(input_manager,spawn(secer_im_server,init,[])),

		{FunName,Arity} = get_function_name(Fun),

		receive
			die -> 
				exit(0);
			continue ->
				ok
		after TimeOut * 1000 ->
				ok
		end,

		input_manager ! {get_results,self()},
		receive
			{Empty,Valued,Same,Different,_Cvg} -> 
				% printer("Empty Trace"),
				% printer(dict:size(Empty)),
				% printer("Valued Trace"),
				% printer(dict:size(Valued)),
				% printer("Same Trace"),
				% printer(dict:size(Same)),
				% printer("Different Trace"),
				% printer(dict:size(Different)),

				case dict:size(Different) of
					0 ->
						io:format("Function: ~s/~s\n",[FunName,Arity]),
						io:format("~s\n",["----------------------------"]),
						io:format("Generated tests: ~p\n",[dict:size(Same)]),
						io:format("Both versions of the program generate identical traces for the point of interest\n"),
						io:format("~s\n",["----------------------------"]);
					X ->
						SameTests = dict:size(Same),
						io:format("Function: ~s/~s\n",[FunName,Arity]),
						io:format("~s\n",["----------------------------"]),
						io:format("Generated tests: ~p\n",[SameTests+X]),
						Percentage = trunc((X/(SameTests+X))*10000)/100,
						io:format("Mismatching tests: ~p (~p%)\n",[X,Percentage]),
						io:format("All mismatching results were saved at: ./results/~s.txt\n",[FunName++"_"++Arity]),
						{ErrorInput,{TraceP1,TraceP2}} = (catch dict:map(fun(K,V) -> throw({K,V}) end, Different)),
						
						% dict:map(
						% 	fun(K,V) -> 
						% 		case V of
						% 			{T1,T2} when length(T1) == length(T2) ->
						% 				printer({K,{T1,T2}}),
						% 				io:get_line("");
						% 			_ ->
						% 				continue
						% 		end
						% 	end, Different),

						io:format("~s\n",["--- First error detected ---"]),
						InputString = lists:flatten(io_lib:format("~w", [ErrorInput])),
						FinalInput = string:substr(InputString,2,length(InputString)-2),
						io:format("Call: ~s(~s)\n",[FunName,FinalInput]),
						ModuleName1 = list_to_atom(filename:basename(File1,".erl")),
						ModuleName2 = list_to_atom(filename:basename(File2,".erl")),
						io:format("~p Trace (~s,~s,~s): ~p\n",[ModuleName1,Line1,Var1,Oc1,TraceP1]),
						io:format("~p Trace (~s,~s,~s): ~p\n",[ModuleName2,Line2,Var2,Oc2,TraceP2]),
						io:format("~s\n",["----------------------------"])
				end,

				{ok,Fd} = file:open("./tmp/Results.txt",[write]),
				io:format(Fd,"~p.\n~p.",[Empty,Valued]),

				ReadeableFileName = "./results/"++FunName++"_"++Arity++".txt",
				{ok,Fd2} = file:open(ReadeableFileName,[write]),
				Mod1 = filename:basename(File1,".erl"),
				Mod2 = filename:basename(File2,".erl"),
				io:format(Fd2,"****************************************\n",[]),
				io:format(Fd2,"EXECUTION ----> {~s_Trace, ~s_Trace}\n",[Mod1,Mod2]),
				io:format(Fd2,"****************************************\n",[]),
				io:format(Fd2,"~s Interest Point: ~s,~s,~s\n",[Mod1,Line1,Var1,Oc1]),
				io:format(Fd2,"~s Interest Point: ~s,~s,~s\n",[Mod2,Line2,Var2,Oc2]),
				io:format(Fd2,"****************************************\n",[]),
				%io:format(Fd2,"DIFFERENT TRACES\n",[]),
				dict:map(fun(K,V) -> 
						InputString0 = lists:flatten(io_lib:format("~w", [K])),
						FinalInput0 = string:substr(InputString0,2,length(InputString0)-2),
						io:format(Fd2,"~s(~s) ----> ~w\n",[FunName,FinalInput0,V]) end,Different);
				% io:format(Fd2,"*** EXECUTION ----> [COMMON TRACE] ***\n",[]),
				% io:format(Fd2,"COMMON TRACES TRACES\n",[]),
				% dict:map(fun(K,V) -> 
				% 		InputString0 = lists:flatten(io_lib:format("~w", [K])),
				% 		FinalInput0 = string:substr(InputString0,2,length(InputString0)-2),
				% 		io:format(Fd2,"~s(~s) ----> ~w\n",[FunName,FinalInput0,V]) end,Same);
				% dict:map(fun(K,V) -> io:format(Fd,"~p\n",[V]) end,Valued),
				
				% printer("Coverage"),
				% printer(Cvg),


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
		TmpM1 = "./tmp/"++filename:basename(File1,".erl")++"Tmp",
		TmpM2 = "./tmp/"++filename:basename(File2,".erl")++"Tmp",

		file:delete(TmpM1++".erl"),
		file:delete(TmpM2++".erl"),
		file:delete(TmpM1++".beam"),
		file:delete(TmpM2++".beam"),
		file:delete(filename:basename(File1,".erl")++".beam")
	end;
run([File,Line,Var,Oc,Fun,Time]) ->
	try 
		TimeOut = list_to_integer(Time),
		register(secer,self()),
		register(input_gen,spawn(secer_input_gen,main,[File,Line,Var,Oc,Fun,TimeOut])),
		register(input_manager,spawn(secer_im_server,init,[])),

		{FunName,Arity} = get_function_name(Fun),

		receive
			die -> 
				exit(0)
		after TimeOut * 1000 ->
				ok
		end,

		input_manager ! {get_results,self()},
		receive
			{Empty,Valued,_,_,_Cvg} -> 
				SizeEmpty = dict:size(Empty),
				SizeValue = dict:size(Valued),
				io:format("Function: ~s/~s\n",[FunName,Arity]),
				io:format("~s\n",["----------------------------"]),
				io:format("Generated tests: ~p\n",[SizeEmpty+SizeValue]),
				case dict:size(Empty) of
					0 ->
						ok;
					SizeE ->
						io:format("Not executing the point of interest: ~p\n",[SizeE])
				end,
				case dict:size(Valued) of
					0 -> 
						ok;
					SizeV ->
						io:format("Executing the point of interest: ~p\n",[SizeV])
				end,
				% printer("Coverage"),
				% printer(Cvg);
				ReadeableFileName = "./results/"++FunName++"_"++Arity++".txt",
				io:format("Results saved in: ~s\n",[ReadeableFileName]),
				io:format("~s\n",["----------------------------"]),
				
				{ok,Fd} = file:open(ReadeableFileName,[write]),

				Mod = filename:basename(File,".erl"),

				io:format(Fd,"****************************************\n",[]),
				io:format(Fd,"EXECUTION ----> ~s_Trace\n",[Mod]),
				io:format(Fd,"****************************************\n",[]),
				io:format(Fd,"~s Interest Point: ~s,~s,~s\n",[Mod,Line,Var,Oc]),
				io:format(Fd,"****************************************\n",[]),
				dict:map(
					fun(K,V) -> 
						InputString = lists:flatten(io_lib:format("~w", [K])),
						FinalInput = string:substr(InputString,2,length(InputString)-2),
						io:format(Fd,"~s(~s) ----> ~w\n",[FunName,FinalInput,V]) 
					end,
					Valued),
				dict:map(
					fun(K,V) -> 
						InputString = lists:flatten(io_lib:format("~w", [K])),
						FinalInput = string:substr(InputString,2,length(InputString)-2),
						io:format(Fd,"~s(~s) ----> ~w\n",[FunName,FinalInput,V]) 
					end,
					Empty);
			X ->
				printer(X),
				printer(error),
				error
		end
	catch 
		_:_ -> error
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
		TmpM1 = "./tmp/"++filename:basename(File,".erl")++"Tmp",
		file:delete(filename:basename(File,".erl")++".beam"),
		file:delete(TmpM1++".erl"),
		file:delete(TmpM1++".beam")
	end.

get_function_name(FunArity) ->
	Tokens = string:tokens(FunArity,"/"),
	[Name,Arity] = Tokens,
	{Name,Arity}.

printer(X) -> io:format("~p\n",[X]).