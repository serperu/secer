-module(secer).
-export([run/5,run_suite/4]).
% PoisRels::[{POIOld,POINew}] 
% CompareFun::fun cf/2
% Fun::"funName/Arity" (String)
% Timeout::integer

%%%%%%%%%%%%%%%%%%%%
%%%% ONLY SUITE %%%%
%%%%%%%%%%%%%%%%%%%%
run_suite(Poi,ExecFun,Timeout,ScriptPath) -> 
	try 
		printS("Generating secer -suite inputs..."),
		case whereis(secer) of
		    undefined ->
		        register(secer, self());
		    _ ->
		        already_started
		end,
		% START SUITE_GEN NODE
		SuiteNode = slave:start(list_to_atom(net_adm:localhost()), 
            							secer_trace_suite, 
            							"-setcookie secer_cookie"),
		% 
		register(input_manager,spawn(secer_im_server,init,[])),
		register(input_gen,spawn(secer_input_gen,main_suite,[Poi,ExecFun,Timeout,ScriptPath])),
		receive
			die -> 
				exit(0);
			continue ->
				ok
		after Timeout * 1000 ->
				ok
		end,
		input_manager ! {get_suite,self()},
		receive
			{InputDict} -> 
			printS("Building test suite..."),
%dict:map(fun(K,V) -> io:format("Input-Trace: ~p => ~p\n",[K,V]) end,InputDict),
				secer_report_constructor:report_suite(InputDict,Poi,ExecFun);
			_ ->

				printer(error),
				error
		end,
		slave:stop(SuiteNode) % REVISAR SI ESTO DA ERROR
	catch 
		E:R -> 
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
		clean_files()
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% RUN SECER COMPARISON %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(PoisRels,ExecFun,Timeout,CompareFun,ScriptPath) -> 
	try 
		case whereis(secer) of
		    undefined ->
		        register(secer, self());
		    _ ->
		        already_started
		end,

		% START SLAVE NODES
		OldNode = slave:start(list_to_atom(net_adm:localhost()), 
            							secer_trace_old, 
            							"-setcookie secer_cookie"),
		NewNode = slave:start(list_to_atom(net_adm:localhost()), 
            							secer_trace_new, 
            							"-setcookie secer_cookie"),

		register(input_manager,spawn(secer_im_server,init,[])),
		register(input_gen,spawn(secer_input_gen,main,[PoisRels,ExecFun,Timeout,CompareFun,ScriptPath])),

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
			{Same,Different,IdPoiDict,Timeouted} -> 
				secer_report_constructor:report(Same,Different,Timeouted,ExecFun);
			_ ->
				printer(error),
				error
		end,
		slave:stop(OldNode),
		slave:stop(NewNode)
	catch 
		E:R -> 
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
		clean_files()
	end.

printer(X) -> io:format("~p\n",[X]).
printS(X) -> io:format("~s\n",[X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SILENT EXECUTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(PoisRels,ExecFun,Timeout,CompareFun,silent) ->
% 	{ok,FdR} = file:open("./tmp/repeat.txt",[append]),
% 	{S,D} = run_silent(PoisRels,ExecFun,Timeout,CompareFun,mutation),
% 	%io:format("~p ~p\n",[S,D]),
%  	%io:format(FdR,"Total:~p Differents:~p Differents(%):~p\n",[S+D,D,trunc((D/(S+D))*10000)/100]).
%  	io:format(FdR,"~p ~p\n",[S+D,D]).

% run(PoisRels,ExecFun,Timeout,CompareFun,silent,random) ->
% 	{ok,FdR} = file:open("./tmp/repeat.txt",[append]),
% 	{S,D} = run_silent(PoisRels,ExecFun,Timeout,CompareFun,random),
% 	%io:format("~p ~p\n",[S,D]),
%  	%io:format(FdR,"Total:~p Differents:~p Differents(%):~p\n",[S+D,D,trunc((D/(S+D))*10000)/100]).
%  	io:format(FdR,"~p ~p\n",[S+D,D]).


% run_silent(PoisRels,ExecFun,Timeout,CompareFun,GenMode) -> 
% 	try 
% 		case whereis(secer) of
% 		    undefined ->
% 		        register(secer, self());
% 		    _ ->
% 		        already_started
% 		end,

% 		register(input_manager,spawn(secer_im_server,init,[])),
% 		case GenMode of
% 			random ->
% 				register(input_gen,spawn(secer_input_gen,main_random,[PoisRels,ExecFun,Timeout,CompareFun]));
% 			mutation ->
% 				register(input_gen,spawn(secer_input_gen,main,[PoisRels,ExecFun,Timeout,CompareFun]))
% 		end,
% 		%{FunName,Arity} = get_function_name(ExecFun),

% 		receive
% 			die -> 
% 				exit(0);
% 			continue ->
% 				ok
% 		after Timeout * 1000 ->
% 				ok
% 		end,
% 		input_manager ! {get_results,self()},
% 		receive
% 			{Same,Different,IdPoiDict,Timeouted} -> 
% 				{dict:size(Same),dict:size(Different)};
% 			_ ->
% 				printer(error),
% 				error
% 		end
% 	catch 
% 		E:R -> 
% 			errorCatch
% 	after
% 		case {whereis(input_gen),whereis(input_manager)} of
% 			{undefined,undefined} ->
% 				ok;
% 			{undefined,Pid} ->
% 				timer:exit_after(0,Pid,kill);
% 			{Pid,undefined} ->
% 				timer:exit_after(0,Pid,kill);
% 			{Pid1,Pid2} ->
% 				timer:exit_after(0,Pid1,kill),
% 				timer:exit_after(0,Pid2,kill)
% 		end,
% 		clean_files()
% 	end.

clean_files() ->
	{ok,Files} = file:list_dir("./tmp/"),
	[ file:delete("./tmp/"++File) || File <- Files ].
