-module(secer_input_gen).
-export([main/5,main_random/5,get_exports/1]). %main/6,

-define(TMP_PATH,"./tmp/").
-define(INPUT_LIMIT,10000).
-define(CUTER_TIMEOUT,20).

main(PoisRels,ExecFun,Time,CMode,CompareFun) ->

	try 
		register(cuterIn,spawn(secer_trace,init,[])),
		register(tracer,spawn(secer_trace,init,[])),
		[{Old,New}|_] = PoisRels,

		FileOld = atom_to_list(element(1,Old)),
		FileNew = atom_to_list(element(1,New)),

		ResComp1 = compile:file(FileOld,[debug_info]),
		ResComp2 = compile:file(FileNew,[debug_info]),

		ModuleName1 = list_to_atom(filename:basename(FileOld,".erl")),
		ModuleName2 = list_to_atom(filename:basename(FileNew,".erl")),

		case {ResComp1,ResComp2} of
			{error,error} ->
				io:format("Module ~p couldn't be compiled due to syntax errors\nModule ~p couldn't be compiled due to syntax errors\n",[ModuleName1,ModuleName2]),
				secer ! die,
				exit(0);
			{error,_} ->
				io:format("Module ~p couldn't be compiled due to syntax errors\n",[ModuleName1]),
				secer ! die,
				exit(0);
			{_,error} ->
				io:format("Module ~p couldn't be compiled due to syntax errors\n",[ModuleName2]),
				secer ! die,
				exit(0);
			_ -> ok
		end,

		{FunName,Arity} = divide_function(ExecFun),
		case Arity of
			0 ->
				instrument_code(PoisRels,CompareFun),
				ModTmp1 = list_to_atom(filename:basename(FileOld,".erl")++"Tmp"),
				ModTmp2 = list_to_atom(filename:basename(FileNew,".erl")++"Tmp"),

				execute_input(ModTmp1,ModTmp2,FunName,[],CMode,Time),
				secer ! continue;
			_ ->
				% PART 1
				{ParamClauses,TypeDicts} = analyze_types(FileOld,FunName,Arity),
				TimeOut = Time div 3,
				Inputs = execute_cuter(ModuleName1,ModuleName2,FunName,ParamClauses,TypeDicts,TimeOut),

				% PART 2
				instrument_code(PoisRels,CompareFun),

				{ok, Fd} = file:open(?TMP_PATH++"cuter.txt", [write]),
				Self = self(),
				Ref = make_ref(),

				spawn(fun() ->
						group_leader(Fd,self()),
						Self ! {cover_compilation(FileOld),Ref}
						end),
				receive
					{_,Ref} ->
						file:close(Fd)
				end,

				ModTmp1 = list_to_atom(filename:basename(FileOld,".erl")++"Tmp"),
				ModTmp2 = list_to_atom(filename:basename(FileNew,".erl")++"Tmp"),

				Queue = lists:foldl(
					fun(I,L) ->
						RefEx = make_ref(),
						{Trace1,Trace2,Equals} = execute_input(ModTmp1,ModTmp2,FunName,I,CMode,TimeOut),
						input_manager ! {existing_trace,I,{Trace1,Trace2},RefEx,self()},
						receive
							{RefEx,true} ->
								L;
							{RefEx,false} ->
								{Pri,Last} = L,
								case Equals of
									true ->
										{Pri,[I|Last]};
									_ ->
										{[I|Pri],Last}
								end
						end
					end,
					{[],[]},
					Inputs),
				Input = 
					case Queue of
						{[PH|PT],_} ->
							PH;
						{[],[LH|LT]} ->
							LH;
						_ ->
							[]
					end,
				case Input of
					[] ->
						gen_random_inputs(ModTmp1,ModTmp2,FunName,ParamClauses,TypeDicts,0,CMode,TimeOut);
					_ ->
						{Clause,Dic} = identify_clause_input(ParamClauses,TypeDicts,Input),
						validate_input(ModTmp1,ModTmp2,FunName,Queue,Clause,Dic,CMode,TimeOut),
						gen_random_inputs(ModTmp1,ModTmp2,FunName,ParamClauses,TypeDicts,0,CMode,TimeOut)
				end
		end
	catch 
		E:R ->
			%printer({E,R}),
			{E,R}
	after
		case whereis(cuterIn) of
			undefined -> 
				ok;
			_ -> 
				unregister(cuterIn),
				unregister(tracer)
		end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% FULL RANDOM %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main_random(PoisRels,ExecFun,Time,CMode,CompareFun) ->
	try 
		register(cuterIn,spawn(secer_trace,init,[])),
		register(tracer,spawn(secer_trace,init,[])),
		[{Old,New}|_] = PoisRels,

		FileOld = atom_to_list(element(1,Old)),
		FileNew = atom_to_list(element(1,New)),

		ResComp1 = compile:file(FileOld,[debug_info]),
		ResComp2 = compile:file(FileNew,[debug_info]),

		ModuleName1 = list_to_atom(filename:basename(FileOld,".erl")),
		ModuleName2 = list_to_atom(filename:basename(FileNew,".erl")),

		case {ResComp1,ResComp2} of
			{error,error} ->
				io:format("Module ~p couldn't be compiled due to syntax errors\nModule ~p couldn't be compiled due to syntax errors\n",[ModuleName1,ModuleName2]),
				secer ! die,
				exit(0);
			{error,_} ->
				io:format("Module ~p couldn't be compiled due to syntax errors\n",[ModuleName1]),
				secer ! die,
				exit(0);
			{_,error} ->
				io:format("Module ~p couldn't be compiled due to syntax errors\n",[ModuleName2]),
				secer ! die,
				exit(0);
			_ -> ok
		end,

		{FunName,Arity} = divide_function(ExecFun),
		case Arity of
			0 ->
				instrument_code(PoisRels,CompareFun),

				ModTmp1 = list_to_atom(filename:basename(FileOld,".erl")++"Tmp"),
				ModTmp2 = list_to_atom(filename:basename(FileNew,".erl")++"Tmp"),

				execute_input(ModTmp1,ModTmp2,FunName,[],CMode,Time);
			_ ->
				% PART 1
				{ParamClauses,TypeDicts} = analyze_types(FileOld,FunName,Arity),
				TimeOut = Time div 3,
				Inputs = execute_cuter(ModuleName1,ModuleName2,FunName,ParamClauses,TypeDicts,TimeOut),

				% PART 2
				instrument_code(PoisRels,CompareFun),

				{ok, Fd} = file:open(?TMP_PATH++"cuter.txt", [write]),
				Self = self(),
				Ref = make_ref(),

				spawn(fun() ->
						group_leader(Fd,self()),
						Self ! {cover_compilation(FileOld),Ref}
						end),
				receive
					{_,Ref} ->
						file:close(Fd)
				end,

				ModTmp1 = list_to_atom(filename:basename(FileOld,".erl")++"Tmp"),
				ModTmp2 = list_to_atom(filename:basename(FileNew,".erl")++"Tmp"),
				[execute_input(ModTmp1,ModTmp2,FunName,I,CMode,TimeOut) || I <- Inputs],
				gen_random_proper_inputs(ModTmp1,ModTmp2,FunName,ParamClauses,TypeDicts,0,CMode,TimeOut)
					
				% {Clause,Dic} = identify_clause_input(ParamClauses,TypeDicts,Input),
				% validate_input(ModTmp1,ModTmp2,FunName,Queue,Clause,Dic,CMode,TimeOut),
				% gen_random_inputs(ModTmp1,ModTmp2,FunName,ParamClauses,TypeDicts,0,CMode,TimeOut)
		end
	catch 
		E:R ->
			%printer({E,R}),
			{E,R}
	after
		case whereis(cuterIn) of
			undefined -> 
				ok;
			_ -> 
				unregister(cuterIn),
				unregister(tracer)
		end
	end.

% main(ProgramName,Line,Var,Oc,Function,Time) ->
% 	try 
% 		register(cuterIn,spawn(secer_trace,init,[])),
% 		register(tracer,spawn(secer_trace,init,[])),

% 		ModuleName = list_to_atom(filename:basename(ProgramName,".erl")),
% 		{FunName,Arity} = divide_function(Function),

% 		case Arity of
% 			0 ->
% 				instrument_code(ProgramName,list_to_integer(Line),Var,list_to_integer(Oc)),
% 				ModTmp = list_to_atom(filename:basename(ProgramName,".erl")++"Tmp"),
% 				execute_input(ModTmp,FunName,[],0);
% 			_ ->
% 				% PART 1
% 				{ParamClauses,TypeDicts} = analyze_types(ProgramName,FunName,Arity),
% 				TimeOut = Time div 3 * 2,
% 				Inputs = execute_cuter(ModuleName,FunName,ParamClauses,TypeDicts,TimeOut),

% 				% PART 2
% 				instrument_code(ProgramName,list_to_integer(Line),Var,list_to_integer(Oc)),
				
% 				{ok, Fd} = file:open(?TMP_PATH++"cuter.txt", [write]),
% 				Self = self(),
% 				Ref = make_ref(),
% 				spawn(fun() ->
% 						group_leader(Fd,self()),
% 						Self ! {cover_compilation(ProgramName),Ref}
% 						end),
% 				receive
% 					{_,Ref} ->
% 						file:close(Fd)
% 				end,

% 				ModTmp = list_to_atom(filename:basename(ProgramName,".erl")++"Tmp"),
% 				lists:map(
% 					fun(I) ->
% 						catch apply(ModuleName,FunName,I),
% 						Cvg = get_coverage(ModuleName),
% 						{Clause,Dic} = identify_clause_input(ParamClauses,TypeDicts,I),
% 						validate_input(ModuleName,FunName,I,Clause,Dic)
% 						%execute_input(ModTmp,FunName,I,Cvg)
% 					end,
% 					Inputs),
% 				gen_random_inputs(ModuleName,empty,FunName,ParamClauses,TypeDicts,0)
% 		end
% 	catch 
% 		E:R ->
% 			{E,R}
% 	after
% 		case whereis(cuterIn) of
% 			undefined -> 
% 				ok;
% 			_ -> 
% 				unregister(cuterIn),
% 				unregister(tracer)
% 		end
% 	end.

%%%%% MAGIC TOOL %%%%%

% magicToolMain(ProgramName,Line,Var,Oc) ->
% 	ExportedFuns = get_exports(ProgramName),
% 	[magicToolMain(ProgramName,Line,Var,Oc,Function)|| Function <- ExportedFuns].

% magicToolMain(ProgramName,Line,Var,Oc,Function) ->
% 	try 
% 		register(cuterIn,spawn(secer_trace,init,[])),

% 		ModuleName = list_to_atom(filename:basename(ProgramName,".erl")),
% 		{FunName,Arity} = divide_function(Function),

% 		case Arity of
% 			0 ->
% 				instrument_code(ProgramName,list_to_integer(Line),Var,list_to_integer(Oc)),
% 				ModTmp = list_to_atom(filename:basename(ProgramName,".erl")++"Tmp"),
% 				execute_input(ModTmp,FunName,[],0);
% 			_ ->
% 				% PART 1
% 				{ParamClauses,TypeDicts} = analyze_types(ProgramName,FunName,Arity),
% 				Inputs = execute_cuter(ModuleName,FunName,ParamClauses,TypeDicts,?CUTER_TIMEOUT),

% 				% PART 2
% 				instrument_code(ProgramName,list_to_integer(Line),Var,list_to_integer(Oc)),
				
% 				{ok, Fd} = file:open(?TMP_PATH++"cuter.txt", [write]),
% 				Self = self(),
% 				Ref = make_ref(),
% 				spawn(fun() ->
% 						group_leader(Fd,self()),
% 						Self ! {cover_compilation(ProgramName),Ref}
% 						end),
% 				receive
% 					{_,Ref} ->
% 						file:close(Fd)
% 				end,

% 				ModTmp = list_to_atom(filename:basename(ProgramName,".erl")++"Tmp"),
% 				lists:map(
% 					fun(I) ->
% 						catch apply(ModuleName,FunName,I)
% 					end,
% 					Inputs),
% 				gen_inputs_until_max_coverage(ModuleName,FunName,Inputs,ParamClauses,TypeDicts,length(Inputs))
% 		end
% 	catch 
% 		E:R ->
% 			{E,R}
% 	after
% 		case whereis(cuterIn) of
% 			undefined -> 
% 				ok;
% 			_ -> 
% 				unregister(cuterIn)
% 		end
% 	end.

% gen_inputs_until_max_coverage(_,_,Inputs,_,_,?INPUT_LIMIT) ->
% 	Inputs;
% gen_inputs_until_max_coverage(ModuleName,FunName,Inputs,ParamClauses,TypeDicts,N) ->
% 	Cvg = get_coverage(ModuleName),
% 	case Cvg of
% 		1.0 ->
% 			Inputs;
% 		_ ->
% 			Params = lists:nth(rand:uniform(length(ParamClauses)),ParamClauses),
% 			{ok,Dic} = dict:find(Params,TypeDicts),	

% 			Input = (catch generate_instance({Dic,Params})),
% 			case lists:member(Input,Inputs) of
% 				true -> 
% 					gen_inputs_until_max_coverage(ModuleName,FunName,Inputs,ParamClauses,TypeDicts,N);
% 				false ->
% 					catch apply(ModuleName,FunName,Input),
% 					gen_inputs_until_max_coverage(ModuleName,FunName,[Input|Inputs],ParamClauses,TypeDicts,N+1)
% 			end
% 	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyze_types(FileName,FunName,Arity) ->
	ModuleName = list_to_atom(filename:basename(FileName,".erl")),
	% compile:file(ModuleName,[debug_info]),
	{ok,Abstract} = smerl:for_file(FileName),
	Exports = smerl:get_exports(Abstract),
	case is_exported(Exports,FunName,Arity) of
		false -> 
			printer("The selected function is not exported"),
			secer ! die;
		true ->
			FuncTypes = typer_mod:get_type_inside_erl(["--show_exported", FileName]),
			[FunctionSpec] = lists:filter(
					fun(X) -> 
						element(1,X) == FunName 
						andalso 
						element(2,X) == Arity 
					end, 
					FuncTypes),	

			{_Origin,InputTypes} = get_inputs(FunctionSpec),

			% printer(InputTypes), EXAMPLE: [any,{number,{int_rng,0,pos_inf},integer}]

			ParamNames = get_parameters(Abstract,FunName,Arity),
			% printer(ParamNames),
			
			% io:get_line("STOP\n"),
			DictOfDicts = generate_all_clause_dicts(ParamNames,InputTypes),
			{ParamNames,DictOfDicts}
	end.


%%%%% ADDED FOR PropEr VERSION %%%%%
analyze_types(FileName,FunName,Arity,proper) ->
	ModuleName = list_to_atom(filename:basename(FileName,".erl")),
	{ok,Abstract} = smerl:for_file(FileName),
	Exports = smerl:get_exports(Abstract),
	case is_exported(Exports,FunName,Arity) of
		false -> 
			printer("The selected function is not exported"),
			secer ! die;
		true ->
			FuncTypes = typer_mod:get_type_inside_erl(["--show_exported", FileName]),
			[FunctionSpec] = lists:filter(
					fun(X) -> 
						element(1,X) == FunName 
						andalso 
						element(2,X) == Arity 
					end, 
					FuncTypes),	

			{_Origin,InputTypes} = get_inputs(FunctionSpec),
			InputTypes
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% execute_cuter0(ModuleName,FunName,ParamClauses,Dicts,TimeOut) ->
% 	Params = lists:nth(rand:uniform(length(ParamClauses)),ParamClauses),
% 	{ok,Dic} = dict:find(Params,Dicts),

% 	Input = (catch generate_instance({Dic,Params})),
% 	%[Input].
% 	%Input = [[7,9],[7,5]].
	
% 	%CuterInputs = get_cuter_inputs(ModuleName,FunName,Input,TimeOut),
% 	CuterInputs = get_cuter_inputs0(ModuleName,undef,FunName,Input,TimeOut),
% 	[Input|CuterInputs].
execute_cuter(ModuleName1,ModuleName2,FunName,ParamClauses,Dicts,TimeOut) ->
	Params = lists:nth(rand:uniform(length(ParamClauses)),ParamClauses),
	{ok,Dic} = dict:find(Params,Dicts),

	Input = (catch generate_instance({Dic,Params})),
	
	case file:open("./config/nocuter.txt",[read]) of
		{ok,_} ->
			%printer("without cuter"),
			[Input];
		{error,_} ->
			%printer("with cuter"),
			CuterInputs = get_cuter_inputs(ModuleName1,ModuleName2,FunName,Input,TimeOut),
			[Input|CuterInputs]
	end.

instrument_code(PoisRels,CompareFun) -> %TODO =>MEJORABLE<= No hacer reset para luego unregister. Controlar eso
	try
		register(var_gen,spawn(secer_fv_server,init,[])),
		secer_criterion_manager:main(PoisRels,CompareFun),
		var_gen ! reset

	catch 
		E:R ->
			{E,R}
	after
		unregister(var_gen)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% PART 1: GET SPECS, GENERATE 1ST INPUT AND EXECUTE CUTER %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VERIFY IF THE CALLED FUNCTION IS EXPORTED AND GET SPEC %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
divide_function(FunArity) ->
	Tokens = string:tokens(FunArity,"/"),
	[Name,Arity] = Tokens,
	{list_to_atom(Name),list_to_integer(Arity)}.

is_exported([],_,_) -> false;
is_exported([{Name,Arity}|_],Name,Arity) -> true;
is_exported([{_,_}|Rest],Name,Arity) ->
	is_exported(Rest,Name,Arity).

get_exports(FileName) ->
	{ok,Abstract} = smerl:for_file(FileName),
	Exports = smerl:get_exports(Abstract),
	lists:map(
			fun({N,A}) ->
				atom_to_list(N)++"/"++integer_to_list(A)
			end,
			Exports).

% get_executed_function([],_,_) -> 
% 	unexporter;
% get_executed_function([{FunName,Arity,Structure,StringSpec}|_],FunName,Arity) ->
% 	{FunName,Arity,Structure,StringSpec};
% get_executed_function([_|Functions],FunName,Arity) ->
% 	get_executed_function(Functions,FunName,Arity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GET THE TYPES OF THE INPUTS FROM THE SPEC %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_inputs(Function_spec) ->
	{Origin,Inputs} = case Function_spec of
		{_Name,_Arity,{contract,Spec},_String_spec} -> % defined by user
			{contract,_,InputList,_} = Spec,
			{user,InputList};
		{_Name,_Arity,{_Output,InputList},_String_spec} -> 
			{typer,InputList}
	end,
	{Origin,lists:map(
					fun(E) ->
						[Res] = get_types([E],[]),
						Res
					end,
					Inputs)}.

get_types([],L) -> lists:reverse(L);
get_types([H|T],L) ->
	case H of
		none ->
			get_types(T,L);
		_ ->
			case H of
				{_c,union,Set,Type} -> 
					get_types(T,[{union,get_types(Set,[]),Type}|L]);
				{_c,list,Set,Type} ->
					get_types(T,[{list,get_types(Set,[]),Type}|L]);
				{_c,tuple,Set,Type} ->
					get_types(T,[{tuple,get_types(Set,[]),Type}|L]);
				{_c,General_type,Posibilities,Type} -> 
					case Posibilities of
						{_,Set} ->
							get_types(T,[{General_type,Set,Type}|L]);
						Set ->
							get_types(T,[{General_type,Set,Type}|L])
					end;
				Type -> 
					get_types(T,[Type|L])
			end
	end.

type_intersection(Type1,Type2) ->
	T1 = get_type(Type1),
	T2 = get_type(Type2),
	ResType = erl_types:t_inf(T1,T2),
	case ResType of
		none ->
			throw("ERROR: uncompatible spec types");
		_ ->
			lists:nth(1,get_types([ResType],[]))
	end.

get_type(Type) ->
	case Type of		
		{number,any,integer} -> 	
			erl_types:t_integer();
		{number,any,unknown} -> 	
			erl_types:t_number();
		{number,{int_rng,Min,Max},integer} ->
			erl_types:t_from_range(Min,Max);
		{number,List,integer} ->
			erl_types:t_integers(List); 	
		{atom,any,unknown} ->
			erl_types:t_atom();	
		{atom,List,unknown} ->
			erl_types:t_atoms(List);
		{list,[TypeHead,{nil,[],unknown}],nonempty} -> 	
			erl_types:t_nonempty_list(get_type(TypeHead));
		{list,[TypeHead,{nil,[],unknown}],unknown} -> 	
			erl_types:t_list(get_type(TypeHead));
		{list,[TypeHead,any],nonempty} ->
			erl_types:t_nonempty_list(get_type(TypeHead));
		{list,[TypeHead,any],unknown} -> 
			erl_types:t_list(get_type(TypeHead));	
		{tuple,List,_} -> 	
			erl_types:t_tuple(lists:map(fun get_type/1,List));
		{union,List,unknown} -> 
			erl_types:sup(List);
		{nil,[],unknown} ->	
			erl_types:t_nil();
		any -> 
			erl_types:t_any();
		{binary,_,unknown} ->
			erl_types:t_binary();
		_ -> 
			throw("Non contempled type")
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GET LIST OF PARAMETERS FROM AST %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_parameters(AST,FunName,Arity) ->
	{_,_,_,_,Funs,_} = AST,
	{function,_Line,FunName,Arity,Clauses} = lists:foldl(
		fun (X,Acc) ->
			case X of
				{_,_,FunName,Arity,_} -> X;
				_ -> Acc
			end
		end,
		{},
		Funs),
	case length(Clauses) of
		1 ->	% ONLY 1 CLAUSE
			[Clause] = Clauses,
			{clause,_,Params,_,_} = Clause,
			[lists:map(fun get_param_name/1,Params)];
		N ->	% TODO MORE THAN 1 CLAUSE ¿?
			[begin
				Clause = lists:nth(Index,Clauses),
				{clause,_,Params,_,_} = Clause,
				lists:map(fun get_param_name/1,Params)
			end || Index <- lists:seq(1,N)]
	end.

get_param_name(P) ->
	case erl_syntax:type(P) of
		integer ->
			erl_syntax:integer_value(P);
		atom ->
			erl_syntax:atom_value(P);
		float ->
			erl_syntax:float_value(P);
		variable -> 
			erl_syntax:variable_name(P);
		string ->
			erl_syntax:string_value(P);
		tuple -> 
			Items = erl_syntax:tuple_elements(P),
			list_to_tuple(lists:map(fun get_param_name/1, Items));
		list ->
			Head = erl_syntax:list_head(P),
			Tail = erl_syntax:list_tail(P),
			{get_param_name(Head),get_param_name(Tail)};
		cons ->
			Head = erl_syntax:list_head(P),
			Tail = erl_syntax:list_tail(P),
			{get_param_name(Head),get_param_name(Tail)};
		Other ->
			Other
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAP PARAMETERS AND TYPES %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_all_clause_dicts(ParamNames,ParamTypes) ->
	% printer(ParamNames),
	% printer(ParamTypes),
	% io:get_line("STOP\n"),
	NewList = lists:map(
			fun (ParamList) ->
				{ParamList,join_names_types(ParamList,ParamTypes,dict:new())}
			end,
			ParamNames),
	dict:from_list(NewList).

join_names_types([],[],Dic) -> 
	Dic;
join_names_types([Name|Names],[Type|Types],Dic) ->
	case Name of
		_ when is_tuple(Name) -> %PARA LISTAS Y TUPLAS
			{Structure,NameTypes,_} = Type,
			case Structure of
				tuple ->
					NewDic = dict:store(Name,tuple,Dic),
					NewDic2 = join_names_types(tuple_to_list(Name),NameTypes,NewDic),
					join_names_types(Names,Types,NewDic2);
				list ->
					NewDic = dict:store(Name,list,Dic),
					[HeadType,TailType] = NameTypes,
					NewDic2 = join_names_types_lists(Name,{HeadType,TailType},NewDic),
					join_names_types(Names,Types,NewDic2);
				union ->
					TupleNameType = get_tuple_type(NameTypes),
					NewDic = dict:store(Name,tuple,Dic),
					{_,TupleElemTypes,_} = TupleNameType,
					NewDic2 = join_names_types(tuple_to_list(Name),TupleElemTypes,NewDic),
					join_names_types(Names,Types,NewDic2)
			end;
		_ ->
			NewDic = case Name of
				underscore ->
					Dic;
				_ ->
					case dict:find(Name,Dic) of
						{ok,PreviousType} ->
							NewType = type_intersection(PreviousType,Type),
							dict:store(Name,NewType,Dic);
						error ->
							dict:store(Name,Type,Dic)
					end
			end,
			join_names_types(Names,Types,NewDic)
	end.

join_names_types_lists(Name,{HType,TType},Dic) ->
	case Name of
		{Head,Tail} when is_tuple(Tail) ->
			NewDic = join_names_types([Head],[HType],Dic),
			join_names_types_lists(Tail,{HType,TType},NewDic);
		{Head,Tail} ->
			NewDic = join_names_types([Head],[HType],Dic),
			join_names_types([Tail],[TType],NewDic)
	end.	

get_tuple_type(ListTypes) ->
	lists:foldl(
		fun(E,Acc) ->
			case E of
				{tuple,_,_} ->
					E;
				_ ->
					Acc
			end
		end,
		0,
		ListTypes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE AN INSTANCE OF THE PARAMETERS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_instance({TypeDic,ParamList}) ->
	VarDic = dict:new(),
	{Instance,_} = lists:mapfoldl(fun generate_instance/2, {TypeDic,VarDic}, ParamList),
	Instance.

generate_instance(Name,{TypeDic,VarDic}) ->
	case Name of
		underscore ->
			{element(2,proper_gen:pick(proper_types:exactly(unused))),{TypeDic,VarDic}};
		_ when is_list(Name) ->
			Instance = element(2,proper_gen:pick(
				proper_types:fixed_list(
					[proper_types:exactly(Char)||Char <- Name]))),
			NewVarDic = dict:store(Name,Instance,VarDic),
			{Instance,{TypeDic,NewVarDic}};
		_ when is_tuple(Name) ->
			{ok,Type} = dict:find(Name,TypeDic),
			case Type of
				tuple ->
					{TupleElems,{_,NewVarDic}} = lists:mapfoldl(fun generate_instance/2, {TypeDic,VarDic}, tuple_to_list(Name)),
					{element(2,proper_gen:pick(
								proper_types:fixed_list(list_to_tuple(TupleElems)))),{TypeDic,NewVarDic}};
				list ->
					{{ListElems,TailElem},NewVarDic} = generate_list_elems(Name,TypeDic,VarDic,[]),
					{element(2,proper_gen:pick(
						proper_types:fixed_list(lists:append(ListElems,TailElem)))),{TypeDic,NewVarDic}}
			end;
		_ -> 
			case dict:find(Name,VarDic) of
					{ok,Instance} -> 
						{Instance,{TypeDic,VarDic}};
					error -> 
						case is_variable(Name) of
							true -> 
								{ok,Type} = dict:find(Name,TypeDic),
								Instance = element(2,proper_gen:pick(generate_proper_type(Type))),
								NewVarDic = dict:store(Name,Instance,VarDic),
								{Instance,{TypeDic,NewVarDic}};
							false ->
								{ok,Type} = dict:find(Name,TypeDic),
								Instance = element(2,proper_gen:pick(generate_proper_type(Type))),
								{Instance,{TypeDic,VarDic}}	
						end
			end	
	end.		

generate_list_elems({Head,Tail},TypeDic,VarDic,Res) ->
	{Instance,{_,NewVarDic}} = generate_instance(Head,{TypeDic,VarDic}),
	case is_tuple(Tail) of
		true ->
			generate_list_elems(Tail,TypeDic,NewVarDic,[Instance|Res]);
		false ->
			{TailInstance,{_,NewVarDic2}} = generate_instance(Tail,{TypeDic,NewVarDic}),
			{{lists:reverse([Instance|Res]),TailInstance},NewVarDic2}	
	end.

generate_proper_type(Type) ->
	case Type of
		{number,any,integer} -> 	
			proper_types:int();
		{number,any,unknown} -> 	
			proper_types:number();
		{number,{int_rng,neg_inf,pos_inf},integer} ->
			proper_types:integer(inf,inf);
		{number,{int_rng,neg_inf,Max},integer} ->
			proper_types:integer(inf,Max);
		{number,{int_rng,Min,pos_inf},integer} ->
			proper_types:integer(Min,inf);
		{number,{int_rng,Min,Max},integer} ->
			proper_types:integer(Min,Max);
		{number,List,integer} -> 
			proper_types:union([proper_types:integer(X,X) || X <- List]);
		{number,List,unknown} -> 	
			proper_types:union([proper_types:exactly(X) || X <- List]);
		{atom,any,unknown} ->	
			proper_types:atom();
		{atom,List,unknown} ->	
			proper_types:union([proper_types:exactly(Atom)|| Atom <- List]);
		{list,[TypeHead,{nil,[],unknown}],nonempty} -> 	
			proper_types:non_empty(
				proper_types:list(generate_proper_type(TypeHead)));
		{list,[TypeHead,{nil,[],unknown}],unknown} -> 	
			proper_types:list(generate_proper_type(TypeHead));
		{list,[_TypeHead,any],nonempty} -> % TODO REVISAR ESTAS DOS CLAUSULAS
			proper_types:non_empty(proper_types:list());
		{list,[_TypeHead,any],unknown}	->	
			proper_types:list();
		{tuple,List,_} -> 	
			proper_types:fixed_list(list_to_tuple(lists:map(fun generate_proper_type/1, List)));
		{union,List,unknown} -> 	
			proper_types:union(lists:map(fun generate_proper_type/1, List));
		{nil,[],unknown} ->	
			proper_types:exactly([]);
		{binary,_,unknown} ->
			proper_types:binary();
		any ->	
			proper_types:any()
	end.

is_variable(N) ->
	case is_atom(N) of
		false ->
			false;
		true ->
			SN = atom_to_list(N),
			case string:substr(SN,1,1) of
				"_" -> 
					true;
				[X] when X >= 65 andalso X =< 90 ->
					true;
				_ ->
					false
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXECUTE CUTER AND GET THE GENERATED INPUTS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_cuter_inputs0(_,ModuleName,FunName,Input,TimeOut) ->
% 	compile:file(ModuleName,[debug_info]),
% 	Self = self(),
% 	Ref = make_ref(),
% 	{ok, Fd} = file:open(?TMP_PATH++"cuter.txt", [write]),
% 	register(cuterProcess,spawn(
% 		fun() ->
% 			group_leader(Fd, self()),
% 			catch cuter:run(ModuleName,FunName,Input,25,[{number_of_pollers,1},{number_of_solvers,1}]),
% 			Self ! {finish,Ref}
% 		end)),
% 	Result = receive
% 		{finish,Ref} ->
% 			RefC = make_ref(),
% 			cuterIn ! {get_results,RefC,Self},
% 			cuterIn ! exit,
% 			receive
% 				{RefC,[]} ->	
% 					[];
% 				{RefC,Inputs} ->
% 					Inputs
% 			end
% 	after TimeOut * 1000 -> 
% 			timer:exit_after(0,cuterProcess,kill),
% 			os:cmd("rm -Rf ./temp"),
% 			RefCut = make_ref(),
% 			cuterIn ! {get_results,RefCut,Self},
% 			receive
% 				{RefCut,[]} ->	
% 					[];
% 				{RefCut,Inputs} ->
% 					Inputs
% 			end
% 	end,
% 	file:close(Fd),
% 	Result.

get_cuter_inputs(ModuleName1,ModuleName2,FunName,Input,TimeOut) ->
	Self = self(),
	Ref = make_ref(),
	{ok, Fd} = file:open(?TMP_PATH++"cuter.txt", [write]),
	register(cuterProcess1,spawn(
		fun() ->
			group_leader(Fd, self()),
			catch cuter:run(ModuleName1,FunName,Input,25,[{number_of_pollers,1},{number_of_solvers,1}]),
			cuterProcess2 ! {finish1,Ref}
		end)),
	register(cuterProcess2,spawn(
		fun() ->
			receive
				{finish1,Ref} ->
					group_leader(Fd, self()),
					catch cuter:run(ModuleName2,FunName,Input,25,[{number_of_pollers,1},{number_of_solvers,1}]),
					Self ! {finish2,Ref}
			after TimeOut*1000 ->
				timer:exit_after(0,cuterProcess1,kill),
				group_leader(Fd, self()),
				catch cuter:run(ModuleName2,FunName,Input,25,[{number_of_pollers,1},{number_of_solvers,1}]),
				Self ! {finish2,Ref}
			end
		end)),
	Result = receive
			{finish2,Ref} ->
				RefC = make_ref(),
				cuterIn ! {get_results,RefC,Self},
				cuterIn ! exit,
				receive
					{RefC,Inputs} ->
						Inputs
				end
	after TimeOut * 2000 -> 
			timer:exit_after(0,cuterProcess2,kill),
			os:cmd("rm -Rf ./temp"),
			RefCut = make_ref(),
			cuterIn ! {get_results,RefCut,Self},
			receive
				{RefCut,[]} ->	
					[];
				{RefCut,Inputs} ->
					Inputs
			end
	end,
	file:close(Fd),
	remove_duplicated(Result).

remove_duplicated(List) ->
	sets:to_list(sets:from_list(List)).
    % lists:reverse(lists:foldl(
    %     fun(Elem, Acc) ->
    %         case lists:member(Elem, Acc) of
    %             true ->
    %                 Acc;
    %             false ->
    %                 [Elem] ++ Acc
    %         end
    %     end, [], List
    % )).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% PART 2: INSTRUMENT THE CODE AND RANDOM GENERATION WITH PROPER  %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXECUTE AN INPUT WITH TRACE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% execute_input(Mod,FunName,Input,Cvg) ->
% 	ScValue = execute(Mod,FunName,Input),
% 	input_manager ! {add,Input,ScValue,Cvg},
% 	ScValue.

execute_input(Mod1,Mod2,FunName,Input,Mode,TO) ->
	ScValue1 = (catch execute(Mod1,FunName,Input,TO)),
	ScValue2 = (catch execute(Mod2,FunName,Input,TO)),
	case {ScValue1,ScValue2} of
		{timeouted,timeouted} ->
			input_manager ! {add,Input,timeouted,Mode},
			{timeouted,timeouted,false};
		{_,timeouted} ->
			input_manager ! {add,Input,ScValue1,timeouted,Mode},
			{ScValue1,timeouted,false};
		{timeouted,_} ->
			input_manager ! {add,Input,timeouted,ScValue2,Mode},
			{timeouted,ScValue2,false};
		_ ->
			RefEq = make_ref(),
			input_manager ! {add,Input,ScValue1,ScValue2,Mode,self(),RefEq},
			receive 
				{RefEq,Equals} ->
					Equals
			end,
			{ScValue1,ScValue2,Equals}
	end.

execute(MP,Fun,Input,TO) ->
	Self = self(),
	Ref = make_ref(),
	tracer ! reset,
	Pid = spawn(
		fun() ->
			catch apply(MP,Fun,Input),
			Self ! {finish,Ref}
		end),
	receive
		{finish,Ref} ->
			RefRes = make_ref(),
			tracer ! {get_results,RefRes,self()},
			receive
				{RefRes,X} -> 
					X
			end
	after TO*10 -> 
			exit(Pid,0),
			timeouted
	end.

% execute(MP,Fun,Input) ->	%EXECUTION WITH TIMEOUT (FOR INFINITE LOOPS)
%     Pid = spawn(MP,Fun,Input),
%     register(exec,Pid),
%     timer:sleep(100), 
%     Pid2 = whereis(exec),
%     case Pid2 of
%         undefined -> 
%             Ref = make_ref(),
% 			tracer ! {get_results,Ref,self()},
% 			receive
% 				{Ref,X} -> X
% 			end;
%         Pid -> 
%             timer:exit_after(0,Pid,kill), 
%             timeouted
%     end.

% execute0(MP,Fun,Input,TO) ->
% 	catch apply(MP,Fun,Input),
% 	Ref = make_ref(),
% 	tracer ! {get_results,Ref,self()},
% 	receive
% 		{Ref,X} -> X
% 	end.

%%%%%%%%%%%%%%%%%%%%
% MEASURE COVERAGE %
%%%%%%%%%%%%%%%%%%%%
cover_compilation(Mod) ->
	cover:start(),
	cover:compile_module(Mod).

get_coverage(Mod) ->
	{ok,CoverageResults} = cover:analyse(Mod,coverage,line),
	Executed = executed_lines(CoverageResults,0),
	Executed/length(CoverageResults).

executed_lines([],Executed) -> Executed;
executed_lines([Line|Remaining],Executed) ->
	LineCov = element(2,Line),
	IsCovered = element(1,LineCov),

	case IsCovered of
		0 -> executed_lines(Remaining,Executed);
		1 -> executed_lines(Remaining,Executed+1)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% RANDOM GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%
% 1 MODULE GENERATION %
%%%%%%%%%%%%%%%%%%%%%%%
% gen_random_inputs(Mod,empty,Fun,ParamClauses,Dicts,N,_) ->
% 	gen_random_inputs(Mod,Fun,ParamClauses,Dicts,N);

% gen_random_inputs(Mod,Fun,ParamClauses,Dicts,10000) ->
% 	NewDicts = dict:map(
% 		fun(_,V) ->
% 			increase_integer_types(V)
% 		end,
% 		Dicts),
% 	gen_random_inputs(Mod,Fun,ParamClauses,NewDicts,0);
% gen_random_inputs(Mod,Fun,ParamClauses,Dicts,N) ->
% 	Params = lists:nth(rand:uniform(length(ParamClauses)),ParamClauses),
% 	{ok,Dic} = dict:find(Params,Dicts),	

% 	Input = (catch generate_instance({Dic,Params})),
% 	Res = validate_input(Mod,Fun,Input,Params,Dic),
% 	case Res of
% 		X when X == contained orelse X == existent_trace ->
% 			gen_random_inputs(Mod,Fun,ParamClauses,Dicts,N+1);
% 		_ ->
% 			gen_random_inputs(Mod,Fun,ParamClauses,Dicts,0)
% 	end.

% validate_input(Mod,Fun,Input,Params,Dic) ->
% 	ModTmp = list_to_atom(atom_to_list(Mod)++"Tmp"),
% 	RefC = make_ref(),
% 	input_manager ! {contained,Input,RefC,self()},
% 	receive
% 		{Ref,true} ->
% 			contained;
% 		{Ref,false} ->
% 			catch apply(Mod,Fun,Input),
% 			Cvg = get_coverage(Mod),
% 			Trace = execute_input(ModTmp,Fun,Input,Cvg),
% 			RefEx = make_ref(),
% 			input_manager ! {existing_trace,Input,Trace,RefEx,self()},
% 			receive
% 				{RefEx,true} ->
% 					existent_trace;
% 				{RefEx,false} ->
% 					gen_guided_input(Mod,Fun,Input,Params,Dic)
% 			end
% 	end.
% gen_guided_input(Mod,Fun,Input,Params,Dic) ->
% 	case length(Input) of
% 		Size when Size < 2 ->
% 			next;
% 		_Size ->
% 			NewInputs = gen_new_inputs(Input,Params,Dic),
% 			[validate_input(Mod,Fun,NewInput,Params,Dic)|| NewInput <- NewInputs]
% 	end. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 MODULES GENERATION COMPARISON %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,10000,Mode) ->
% 	NewDicts = dict:map(
% 		fun(_,V) ->
% 			increase_integer_types(V)
% 		end,
% 		Dicts),
% 	gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,NewDicts,0,Mode);
% gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,N,Mode) ->
% 	Params = lists:nth(rand:uniform(length(ParamClauses)),ParamClauses),
% 	{ok,Dic} = dict:find(Params,Dicts),	

% 	Input = (catch generate_instance({Dic,Params})),
% 	Res = validate_input(Mod1,Mod2,Fun,Input,Params,Dic,Mode),
% 	case Res of
% 		X when X == contained orelse X == existent_trace ->
% 			gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,N+1,Mode);
% 		_ ->
% 			gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,0,Mode)
% 	end.

% validate_input(Mod1,Mod2,Fun,Input,Params,Dic,Mode) ->

% 	RefC = make_ref(),
% 	input_manager ! {contained,Input,RefC,self()},
% 	receive
% 		{RefC,true} ->
% 			contained;
% 		{RefC,false} ->
% 			%catch apply(Mod1,Fun,Input),
% 			%Cvg = get_coverage(Mod1),
% 			{Trace1,Trace2} = execute_input(Mod1,Mod2,Fun,Input,Mode),
% 			RefEx = make_ref(),
% 			input_manager ! {existing_trace,Input,{Trace1,Trace2},RefEx,self()},
% 			receive
% 				{RefEx,true} ->
% 					existent_trace;
% 				{RefEx,false} ->
% 					gen_guided_input(Mod1,Mod2,Fun,Input,Params,Dic,Mode)
% 			end
% 	end.

% gen_guided_input(Mod1,Mod2,Fun,Input,Params,Dic,Mode) ->
% 	case length(Input) of
% 		Size when Size < 2 ->
% 			next;
% 		_Size ->
% 			NewInputs = gen_new_inputs(Input,Params,Dic),
% 			[validate_input(Mod1,Mod2,Fun,NewInput,Params,Dic,Mode)|| NewInput <- NewInputs]
% 	end. 

% gen_new_inputs(Input,Params,Dic) ->
% 	{NewInputs,_} = lists:mapfoldl(
% 		fun(E,Acc) ->
% 			{NewParam,_} = generate_instance(E,{Dic,dict:new()}),
% 			{replacenth(Acc,NewParam,Input),Acc+1}
% 		end,
% 		1,
% 		Params),
% 	NewInputs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GEN FULL RANDOM PROPER   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_random_proper_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,10000,Mode,TO) ->
	NewDicts = dict:map(
		fun(_,V) ->
			increase_integer_types(V)
		end,
		Dicts),
	gen_random_proper_inputs(Mod1,Mod2,Fun,ParamClauses,NewDicts,0,Mode,TO);
gen_random_proper_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,N,Mode,TO) -> %VALIDAR NO REPETIDO?
	Params = lists:nth(rand:uniform(length(ParamClauses)),ParamClauses),
	{ok,Dic} = dict:find(Params,Dicts),	
	Input = (catch generate_instance({Dic,Params})),
	execute_input(Mod1,Mod2,Fun,Input,Mode,TO),
	gen_random_proper_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,0,Mode,TO).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,10000,Mode,TO) ->
	NewDicts = dict:map(
		fun(_,V) ->
			increase_integer_types(V)
		end,
		Dicts),
	gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,NewDicts,0,Mode,TO);
gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,N,Mode,TO) ->
	Params = lists:nth(rand:uniform(length(ParamClauses)),ParamClauses),
	{ok,Dic} = dict:find(Params,Dicts),	
	Input = (catch generate_instance({Dic,Params})),
	Res = validate_input(Mod1,Mod2,Fun,{[Input],[]},Params,Dic,Mode,TO),

	case Res of
		X when X == contained orelse X == existent_trace ->
			gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,N+1,Mode,TO);
		_ ->
			gen_random_inputs(Mod1,Mod2,Fun,ParamClauses,Dicts,0,Mode,TO)
	end.

validate_input(Mod1,Mod2,Fun,Queue,Params,Dic,Mode,TO) ->
	{Input,{Prior,Last}} = case Queue of
		{[],[]} ->
			{finish,{[],[]}};
		{[HP|TP],L} ->
			{HP,{TP,L}};
		{[],[HL|TL]} ->
			{HL,{[],TL}}
	end,
	case Input of
		finish ->
			finish;
		_ ->
			RefC = make_ref(),
			input_manager ! {contained,Input,RefC,self()},
			receive
				{RefC,true} ->
					contained;
				{RefC,false} ->
					{Trace1,Trace2,Equals} = execute_input(Mod1,Mod2,Fun,Input,Mode,TO),
					RefEx = make_ref(),
					input_manager ! {existing_trace,Input,{Trace1,Trace2},RefEx,self()},
					receive
						{RefEx,true} ->
							existent_trace;
						{RefEx,false} ->
							MutatingInputs = gen_guided_input(Mod1,Mod2,Fun,Input,Params,Dic,Mode),
							NewQueue = case Equals of
								true ->
									{Prior,Last ++ MutatingInputs};
								_ ->
									{Prior ++ MutatingInputs, Last}
							end,
							validate_input(Mod1,Mod2,Fun,NewQueue,Params,Dic,Mode,TO)
					end
			end
	end.

gen_guided_input(Mod1,Mod2,Fun,Input,Params,Dic,Mode) ->
	case length(Input) of
		Size when Size < 2 ->
			[];
		_Size ->
			gen_new_inputs(Input,Params,Dic)
	end. 

gen_new_inputs(Input,Params,Dic) ->
	{NewInputs,_} = lists:mapfoldl(
		fun(E,Acc) ->
			{NewParam,_} = generate_instance(E,{Dic,dict:new()}),
			{replacenth(Acc,NewParam,Input),Acc+1}
		end,
		1,
		Params),
	NewInputs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increase_integer_types(Dic) ->
	dict:map(
		fun (_,V) ->
				case V of
					{number,any,integer} ->
						{number,{int_rng,-50,50},integer};
					{number,{int_rng,neg_inf,pos_inf},integer} ->
						{number,{int_rng,neg_inf,pos_inf},integer};
					{number,{int_rng,Min,pos_inf},integer} ->
						case Min < 0 of
							true ->
								{number,{int_rng,2*Min,pos_inf},integer};
							false ->
								{number,{int_rng,Min,pos_inf},integer}
						end;
					{number,{int_rng,neg_inf,Max},integer} ->
						{number,{int_rng,neg_inf,Max*2},integer};
					{number,{int_rng,Min,Max},integer} ->
						case {Min =< 0, Max >= 0} of
							{false,true} ->
								{number,{int_rng,Max,10*(Max-Min)},integer};
							{true,false} ->
								{number,{int_rng,-10*(Min-Max),Max},integer};
							_ ->
								{number,{int_rng,10*Min,10*Max},integer}
						end;
					{tuple,L,Tail} ->
						NewL = [increase_internal_integer_types(Elem) || Elem <- L],
						{tuple,NewL,Tail};
					Value ->
						Value
				end
		end, 
		Dic).
increase_internal_integer_types(TypeList) ->
	case TypeList of
		{number,any,integer} ->
			{number,{int_rng,-50,50},integer};
		{number,{int_rng,neg_inf,pos_inf},integer} ->
			{number,{int_rng,neg_inf,pos_inf},integer};
		{number,{int_rng,Min,pos_inf},integer} ->
			case Min < 0 of
				true ->
					{number,{int_rng,2*Min,pos_inf},integer};
				false ->
					{number,{int_rng,Min,pos_inf},integer}
			end;
		{number,{int_rng,neg_inf,Max},integer} ->
			{number,{int_rng,neg_inf,Max*2},integer};
		{number,{int_rng,Min,Max},integer} ->
			case {Min =< 0, Max >= 0} of
				{false,true} ->
					{number,{int_rng,Max,10*(Max-Min)},integer};
				{true,false} ->
					{number,{int_rng,-10*(Min-Max),Max},integer};
				_ ->
					{number,{int_rng,10*Min,10*Max},integer}
			end;
		{tuple,L,Tail} ->
			NewL = [increase_internal_integer_types(Elem) || Elem <- L],
			{tuple,NewL,Tail};
		Value ->
			Value
	end.

identify_clause_input([Clause],Dicts,_) ->
	{ok,Dict} = dict:find(Clause,Dicts),
	{Clause,Dict};
identify_clause_input([Clause|Clauses],Dicts,Input) ->
	{ok,Dict} = dict:find(Clause,Dicts),
	case is_input(Clause,Dict,Input) of
		true ->
			{ok,Dict} = dict:find(Clause,Dicts),
			{Clause,Dict};
		false ->
			identify_clause_input(Clauses,Dicts,Input)
	end.

is_input([],_,[]) ->
	true;
is_input([Param|Params],Dict,[Arg|Args]) ->
	case Param == Arg of
		true ->
			is_input(Params,Dict,Args);
		false ->
			{ok,Type} = dict:find(Param,Dict),
			case is_valid_type(Param,Type,Dict,Arg) of
				true ->
					is_input(Params,Dict,Args);
				false ->
					false
			end
	end.

is_valid_type(Param,Type,Dict,Arg) ->
	case Type of
		{number,any,integer} -> 	
			is_integer(Arg);
		{number,any,unknown} -> 	
			is_number(Arg);
		{number,{int_rng,neg_inf,pos_inf},integer} ->
			is_integer(Arg);
		{number,{int_rng,neg_inf,Max},integer} ->
			is_integer(Arg) andalso Arg =< Max;
		{number,{int_rng,Min,pos_inf},integer} ->
			is_integer(Arg) andalso Arg >= Min;
		{number,{int_rng,Min,Max},integer} ->
			is_integer(Arg) andalso Arg =< Max andalso Arg >= Min;
		{number,List,integer} ->
			is_integer(Arg) andalso lists:member(Arg,List);
		{atom,any,unknown} ->
			is_atom(Arg);
		{atom,List,unknown} ->
			is_atom(Arg) andalso lists:member(Arg,List);
		{list,_,_} -> 	
			is_list(Arg);
		list ->
			case is_list(Arg) of
				true ->
					possible_list(Param,Dict,Arg);
				false ->
					false
			end;
		{nil,[],unknown} ->	
			Arg == [];	
		{tuple,TypeList,{Lenght,_}} -> 	
			case is_tuple(Arg) of
				true ->
					case tuple_size(Arg) of
						Lenght ->
							possible_tuple(TypeList,tuple_to_list(Arg));
						_ ->
							false
					end;
				false ->
					false
			end;
		tuple ->
			case is_tuple(Arg) of
				true ->
					Length = tuple_size(Param),
					case tuple_size(Arg) of
						Lenght ->
							possible_tuple_param_based(tuple_to_list(Param),Dict,tuple_to_list(Arg));
						_ ->
							false
					end;
				false ->
					false
			end;
		{union,List,unknown} -> 
			ValidTypes = [ is_valid_type(null,PossibleType,null,Arg) || PossibleType <- List],
			lists:any(fun(X) -> X end, ValidTypes);
		any -> 
			true;
		_ -> 
			throw("Non contempled type")
	end.

possible_list({H,T},Dict,[ArgH|ArgT]) when is_tuple(T) == false ->
	{ok,Type} = dict:find(H,Dict),
	case is_valid_type(H,Type,Dict,ArgH) of
		true ->
			case ArgT of
				[] ->
					T == nil;
				_ when is_list(ArgT) == false ->
					{ok,Type2} = dict:find(T,Dict),
					is_valid_type(T,Type2,Dict,ArgT);
				_ ->
					false
			end;
		false ->
			false
	end;
possible_list({H,T},Dict,[ArgH|ArgT]) ->
	{ok,Type} = dict:find(H,Dict),

	case is_valid_type(H,Type,Dict,ArgH) of
		true ->
			possible_list(T,Dict,ArgT);
		false ->
			false
	end.

possible_tuple([],[]) ->
	true;
possible_tuple([H|T],[Arg|Args]) ->
	case is_valid_type(null,H,null,Arg) of
		true ->
			possible_tuple(T,Args);
		false ->
			false
	end.

possible_tuple_param_based([],_,[]) -> 
	true;
possible_tuple_param_based([Param|Params],Dict,[Arg|Args]) ->
	{ok,Type} = dict:find(Param,Dict),
	case is_valid_type(Param,Type,Dict,Arg) of
		true ->
			possible_tuple_param_based(Params,Dict,Args);
		false ->
			false
	end.

%%%%%%%%%%%%
%%% MISC %%%
%%%%%%%%%%%%
replacenth(Index,Value,List) ->
 replacenth(Index-1,Value,List,[],0).

replacenth(ReplaceIndex,Value,[_|List],Acc,ReplaceIndex) ->
 lists:reverse(Acc)++[Value|List];
replacenth(ReplaceIndex,Value,[V|List],Acc,Index) ->
 replacenth(ReplaceIndex,Value,List,[V|Acc],Index+1).

%%%%%%%%%
% DEBUG %
%%%%%%%%%
printer(Node) -> io:format("~p\n",[Node]).

printList([]) -> 
	theEnd;
printList([H|T]) ->
	printer(H),
	printer("<=============>"),
	printList(T).
