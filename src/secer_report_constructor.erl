-module(secer_report_constructor).
-export([report/4,default_report/3]).

report(Same,Different,_Timeouted,ExecFun) ->
		
	{FunName,Arity} = get_function_name(ExecFun),

	io:format("\n"),
	io:format("Function: ~s/~s\n",[FunName,Arity]),
	io:format("~s\n",["----------------------------"]),
	DSize = dict:size(Different),
	SSize = dict:size(Same),
	io:format("Generated test cases: ~p\n",[SSize+DSize]),

	% DIFFERENT GENERATED REPORTS
	case DSize of
		0 ->
			io:format("Both versions of the program generate identical traces for the defined POIs\n"),
			io:format("~s\n",["----------------------------"]);
		_ ->
			Percentage = trunc((DSize/(SSize+DSize))*10000)/100,
			io:format("Mismatching test cases: ~p (~p%)\n",[DSize,Percentage]),
			ErrorDict = error_classifier(Different,FunName),
			io:format("\n"),
			dict:map(fun(ErrorType,Input) ->
						print_error(Input,FunName,Different)
					 end,
					 ErrorDict)
	end.

error_classifier(Dict,FunName) ->
	Errors = dict:fold(fun(K,V,D) ->
						{ErrorType,_} = V,
						dict:append(ErrorType,K,D)
					  end,
					  dict:new(),
					  Dict),

	dict:map(fun(K,V) -> 
				io:format("~p => ~p Errors\n",[K,length(V)]),
				[Input|_] = V,
				InputString = lists:flatten(io_lib:format("~w", [Input])),
				FinalInput = string:substr(InputString,2,length(InputString)-2),
				io:format("\t Example call: ~s(~s)\n",[FunName,FinalInput]),
				Input
			 end,
 	Errors).

print_error(Input,FunName,Different) ->
	{ErrorType,ErrorFun} = dict:fetch(Input,Different),

	io:format("~s\n",["------ Detected Error ------"]),
	InputString = lists:flatten(io_lib:format("~w", [Input])),
	FinalInput = string:substr(InputString,2,length(InputString)-2),
	io:format("Call: ~s(~s)\n",[FunName,FinalInput]),
	io:format("Error Type: ~p\n",[ErrorType]),

	Report = ErrorFun(),
	io:format("~s\n",[Report]),
	io:format("~s\n",["----------------------------"]).

default_report({POE,VOE},{PNE,VNE},His) ->
	{OldValues,NewValues} = 
		lists:foldl(fun({{OE,VO},{NE,VN}},{OVals,NVals}) ->
			case OE == POE andalso NE == PNE of
				true -> {[VO | OVals],[VN | NVals]};
				_ -> {OVals,NVals}
			end
		end,
		{[],[]},
		His),
	FinalOldValues = lists:reverse([VOE | OldValues]),
	FinalNewValues = lists:reverse([VNE | NewValues]),
	lists:flatten(io_lib:format("POI: (~p) trace:\n\t ~w\nPOI: (~p) trace:\n\t ~w\n", 
									[poi_translation(POE),FinalOldValues,poi_translation(PNE),FinalNewValues])).

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


