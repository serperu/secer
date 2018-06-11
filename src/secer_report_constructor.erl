-module(secer_report_constructor).
-export([report/4]).

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
				io:format("~p => ~p Errors\n", [K, length(V)]), 
				[Input|_] = V, 
				InputString = lists:flatten(io_lib:format("~w", [Input])), 
				FinalInput = string:substr(InputString, 2, length(InputString)-2), 
				io:format("\t Example call: ~s(~s)\n", [FunName, FinalInput]), 
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
	io:format("~s\n", ["----------------------------"]),
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


