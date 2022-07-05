-module(secer_report_constructor).
-export([report/4,report_suite/3]).

-define(SUITE_PATH, "./suite/").

report_suite(InputTraceDict, Poi, ExecFun) ->
	% File = atom_to_list(element(1, Poi)), 
	% ModuleName = list_to_atom(filename:basename(File, ".erl")), 
	{FunName, Arity} = get_function_name(ExecFun), 
	filelib:ensure_dir(?SUITE_PATH),
	try 
		{ok, Fd} = file:open(?SUITE_PATH++"suite.txt", [append]),
		io:format(Fd,"\n*** Generated tests for Function ~p POI ~p ***\n\n",[ExecFun,Poi]),
		print_test_cases(Fd,InputTraceDict,FunName),
		file:close(Fd)
	catch 
		_:_ -> 
			io:format("Error writing suite test cases")
	end,
	io:format("Suite successfully built in ~s: ~p different test cases computed\n",
		[?SUITE_PATH++"suite.txt",dict:size(InputTraceDict)]).

print_test_cases(Fd,Dict,FunName) ->
	dict:map(fun(I, T) ->
				InputString = lists:flatten(io_lib:format("~w", [I])), 
				FinalInput = string:substr(InputString, 2, length(InputString)-2), 
				Trace = get_trace_value(T),
				io:format(Fd,"~s(~s) => ~w\n\n",[FunName,FinalInput,Trace])
			end,
			Dict).

get_trace_value(Trace) ->
	VEF = secer_api:vef_value_only(),
	lists:foldr(
 	fun(T,Acc) ->
		[VEF(T)|Acc]
	end,
	[],
	Trace).


report(Same, Different, _Timeouted, ExecFun) ->		
	{FunName, Arity} = get_function_name(ExecFun), 
	Errors = error_classifier(Different), 

	io:format("\n"), 
	io:format("Function: ~s/~s\n", [FunName, Arity]), 
	io:format("~s\n", ["----------------------------"]), 
	DSize = dict:size(Different), 
	SSize = dict:size(Same), 
	io:format("Generated test cases: ~p\n", [SSize+DSize]), 

	% DIFFERENT GENERATED REPORTS
	case DSize of
		0 ->
			io:format("Both versions of the program generate identical traces for the defined POIs\n"), 
			io:format("~s\n", ["----------------------------"]);
		_ ->
			Percentage = trunc((DSize/(SSize+DSize))*10000)/100, 
			io:format("Mismatching test cases: ~p (~p%)\n", [DSize, Percentage]),
			io:format("  Error Types:\n"),
			ErrorDict = error_recount(Errors,FunName),
			io:format("\n"), 
			dict:map(fun(ErrorType, Input) ->
						print_error(Input, FunName, Different)
					 end, 
					 ErrorDict)
	end.

error_classifier(Dict) ->
	dict:fold(fun(K, V, D) ->
						{ErrorType, _} = V, 
						dict:append(ErrorType, K, D)
					  end, 
					  dict:new(), 
					  Dict).

error_recount(Errors, FunName) ->
	dict:map(fun(K, V) -> 
				io:format("    + ~p => ~p Errors\n", [K, length(V)]), 
				[Input|_] = V, 
				InputString = lists:flatten(io_lib:format("~w", [Input])), 
				FinalInput = string:substr(InputString, 2, length(InputString)-2), 
				io:format("        Example call: ~s(~s)\n", [FunName, FinalInput]), 
				Input
			 end, 
 	Errors).

print_error(Input, FunName, Different) ->
	{ErrorType, Error} = dict:fetch(Input, Different), 

	io:format("~s\n", ["------ Detected Error ------"]), 
	InputString = lists:flatten(io_lib:format("~w", [Input])), 
	FinalInput = string:substr(InputString, 2, length(InputString)-2), 
	io:format("Call: ~s(~s)\n", [FunName, FinalInput]), 
	io:format("Error Type: ~p\n", [ErrorType]), 
	io:format("~s\n", ["- - - - - - - - - - - - - - "]),
	Report = case is_function(Error) of
		true ->
			Error();
		false ->
			Error
	end,
	io:format("~s", [Report]),
	io:format("~s\n", ["----------------------------"]).

get_function_name(FunArity) ->
	Tokens = string:tokens(FunArity, "/"), 
	[Name, Arity] = Tokens, 
	{Name, Arity}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% PRINTER FUNCTIONS %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printer(X) -> io:format("~p\n", [X]).


