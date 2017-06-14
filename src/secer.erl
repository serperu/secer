-module(secer).
-export([run/1]).
run([File1,Line1,Var1,Oc1,File2,Line2,Var2,Oc2,Fun,Time]) ->
	try 
		TimeOut = list_to_integer(Time),
		register(secer,self()),
		register(input_gen,spawn(secer_input_gen,main,[File1,Line1,Var1,Oc1,File2,Line2,Var2,Oc2,Fun,TimeOut])),
		register(input_manager,spawn(secer_im_server,init,[])),

		receive
			die -> 
				exit(0)
		after TimeOut * 1000 ->
				ok
		end,

		input_manager ! {get_results,self()},
		receive
			{Empty,Valued,Same,Different,Cvg} -> 
				printer("Empty inputs"),
				printer(dict:size(Empty)),
				printer("Valued inputs"),
				printer(dict:size(Valued)),

				{ok,Fd} = file:open("./tmp/Results.txt",[write]),
				io:format(Fd,"~p.\n~p.",[Empty,Valued]),

				{ok,Fd2} = file:open("./tmp/ReadeableResults.txt",[write]),
				io:format(Fd2,"*** INPUT ----> VALUE ***\n",[]),
				dict:map(fun(K,V) -> io:format(Fd2,"~w ----> ~w\n",[K,V]) end,Empty),
				dict:map(fun(K,V) -> io:format(Fd2,"~w ----> ~w\n",[K,V]) end,Valued),
				%dict:map(fun(K,V) -> io:format(Fd,"~p\n",[V]) end,Valued),
				
				printer("Same Value"),
				printer(dict:size(Same)),
				printer("Different Value"),
				printer(dict:size(Different)),
				% printer("Coverage"),
				% printer(Cvg),

				case dict:size(Different) of
					0 ->
						printer("All tests returned the same value");
					X ->
						SameTests = dict:size(Same),
						io:format("The number of generated tests is: ~p\n",[SameTests+X]),
						io:format("The number of incorrect tests is: ~p\n",[X]),

						{ErrorInput,{TraceP1,TraceP2}} = (catch dict:map(fun(K,V) -> throw({K,V}) end, Different)),
						
						io:format("~s\n",["--- First error detected ---"]),
						io:format("Input: ~p\n",[ErrorInput]),
						ModuleName1 = list_to_atom(filename:basename(File1,".erl")),
						ModuleName2 = list_to_atom(filename:basename(File2,".erl")),
						io:format("~p results: ~p\n",[ModuleName1,TraceP1]),
						io:format("~p results: ~p\n",[ModuleName2,TraceP2])
				end;
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
		end
	end;
run([File,OffsetStart,OffsetEnd,Fun,Time]) ->
	try 
		TimeOut = list_to_integer(Time),
		register(secer,self()),
		printer(whereis(secer)),
		register(input_gen,spawn(secer_input_gen,main,[File,OffsetStart,OffsetEnd,Fun,TimeOut])),
		register(input_manager,spawn(secer_im_server,init,[])),

		receive
			die -> 
				exit(0)
		after TimeOut * 1000 ->
				ok
		end,

		input_manager ! {get_results,self()},
		receive
			{Empty,Valued,_,_,Cvg} -> 
				printer("Empty inputs"),
				printer(dict:size(Empty)),
				printer("Valued inputs"),
				printer(dict:size(Valued)),
				printer("Coverage"),
				printer(Cvg);
				% Save results in projects/default/default.txt file
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
		end
	end.

printer(X) -> io:format("~p\n",[X]).