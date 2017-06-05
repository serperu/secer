-module(secer_input_gen).
-export([main/2]).

-define(TIMEOUT,20000).
-define(MIN_COV,0.9).
-define(MAX_TESTS,1000).
% -define(RELATIVE_PATH,"../tmp/").
% -define(RETURN_PATH,"../test_gen").
-define(FILE_PATH,"../").
-define(TESTGEN_PATH,"./ebin").

main(ProgramName,Function) ->
	try 
		code:add_path(filename:absname("../proper/ebin")),
		code:add_path(filename:absname("../cuter/ebin")),

		code:purge(secer_trace),
		code:load_abs("secer_trace"),

		register(cuterIn,spawn(secer_trace,init,[])),
		FileName = ?FILE_PATH++atom_to_list(ProgramName),

		main0(FileName,Function)
	catch 
		E:R ->
			{E,R}
	after
		case whereis(cuterIn) of
			undefined -> ok;
			_ -> unregister(cuterIn)
		end
	end.

main0(FileName,FunName) ->
	ModuleName = list_to_atom(filename:basename(?FILE_PATH++FileName,".erl")),

	c:cd(?FILE_PATH),
	compile:file(ModuleName,[debug_info]),
	c:cd(?TESTGEN_PATH),
	io:get_line("compilado y funcionando"),
	{ok,Abstract} = smerl:for_file(FileName),
	Exports = smerl:get_exports(Abstract),
	case is_exported(Exports,FunName) of
		false -> 
			throw("Unexported function");
		true ->
			FuncTypes = typer:get_type_inside_erl(["--show_exported", FileName]),
			ExportedFunctionsTypes = lists:reverse(exported(Exports,FuncTypes,[])),
			FunctionSpec = get_executed_function(ExportedFunctionsTypes,FunName),
			{Origin,InputTypes} = get_inputs(FunctionSpec),
			ParamNames = get_parameters(Abstract,FunName),

			ParamList = lists:nth(rand:uniform(length(ParamNames)),ParamNames), 

			Dic = dict:new(),
			NewDic = join_names_types(ParamList,InputTypes,Dic),

			Input = (catch generate_instance({NewDic,ParamList})),
			%printer(Input),

			CuterInputs = get_cuter_inputs(ModuleName,FunName,Input),
			
			% exit(CuterInputs),

			{FinalInputs,Coverage} = case CuterInputs of
				cuterError ->
					printer("Error de cuter"),
					cover_compilation(ModuleName),
					gen_and_cover(?MAX_TESTS,NewDic,ParamList,ModuleName,FunName,0,{[Input],0});
				_ ->
					CuterIn = [Input|CuterInputs],
					Cvg = measure_coverage_cuter(CuterIn,ModuleName,FunName),
					case Cvg of
						X when X < ?MIN_COV ->
							gen_and_cover(?MAX_TESTS - length(CuterIn),NewDic,ParamList,ModuleName,FunName,0,{CuterIn,Cvg});
						_ ->
							cover:stop(),
							{CuterIn,Cvg}
					end
			end
			% printer({length(FinalInputs),Coverage}),
			% printList(FinalInputs)
	end.

%%%%%%%%%%%%%%%%%%%%
% GET CUTER INPUTS %
%%%%%%%%%%%%%%%%%%%%
get_cuter_inputs(ModuleName,FunName,Input) ->
	Self = self(),
	spawn(
		fun() ->
			c:cd(?FILE_PATH),
			catch cuter:run(ModuleName,FunName,Input,25,[{number_of_pollers,1},{number_of_solvers,1}]),
			c:cd(?TESTGEN_PATH),
			Self ! finish
		end),

	receive
		finish ->
			cuterIn ! {get_results,Self},
			cuterIn ! exit,
			receive
				[] ->	%QUE NO GENERE VALORES NO TIENE POR QUE IMPLICAR QUE NO PUDO RESOLVERLO
					cuterError;
				Inputs ->
					Inputs
			end;
		X ->
			throw({"Unexpected reception",X})
	after ?TIMEOUT ->
			cuterIn ! {get_results,Self},
			receive
				[] ->	%QUE NO GENERE VALORES NO TIENE POR QUE IMPLICAR QUE NO PUDO RESOLVERLO
					cuterError;
				Inputs ->
					Inputs
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEASURE COVERAGE CUTER %
%%%%%%%%%%%%%%%%%%%%%%%%%%
measure_coverage_cuter(Inputs,Mod,FunName) ->
	cover:start(),
	cover:compile_module(?FILE_PATH++atom_to_list(Mod)++".erl"),
	execute(Inputs,Mod,FunName),
	getCoverage(Mod).

getCoverage(Mod) ->
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

execute([],_,_) -> stop;
execute([Input|Rest],M,F) ->
	catch apply(M,F,Input),
	execute(Rest,M,F).

% execute([Input|Rest],M,F) ->
%     Pid = spawn(M,F,Input),
%     register(test,Pid),
%     timer:sleep(60), 
%     Pid2 = whereis(test),
%     case Pid2 of
%         undefined -> 
%             finished;
%         Pid -> 
%             timer:exit_after(0,Pid,kill), 
%             finished
%     end,
%     execute(Rest,M,F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEASURE COVERAGE PROPER %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
cover_compilation(Mod) ->
	cover:start(),
	cover:compile_module(?FILE_PATH++atom_to_list(Mod)++".erl").

gen_and_cover(0,_,_,_,_,_,Res) -> 
	cover:stop(),
	Res;
gen_and_cover(N,TypeDic,Param,Mod,FunName,NoInputTimes,{Inputs,_}) ->
	Input = generate_instance({TypeDic,Param}),
	execute(Inputs,Mod,FunName),
	Cvg = getCoverage(Mod),
	case Cvg of
		Cvg when Cvg > ?MIN_COV ->
			gen_and_cover(0,TypeDic,Param,Mod,FunName,0,{[Input|Inputs],Cvg});
		_ ->
			case {lists:member(Input,Inputs),NoInputTimes} of
				{true,Times} when Times > 20 ->
					NewTypeDic = increment_integer_types(TypeDic),
					gen_and_cover(N,NewTypeDic,Param,Mod,FunName,0,{Inputs,Cvg});
				{true,_} ->
					gen_and_cover(N,TypeDic,Param,Mod,FunName,NoInputTimes+1,{Inputs,Cvg});
				{false,_} ->
					gen_and_cover(N-1,TypeDic,Param,Mod,FunName,0,{[Input|Inputs],Cvg})
			end
	end.

increment_integer_types(Dic) ->
	dict:map(
		fun (K,V) ->
				case V of
					{number,any,integer} ->
						{number,{int_rng,-100,100},integer};
					{number,{int_rng,Min,Max},integer} ->
						{number,{int_rng,10*Min,10*Max},integer};
					Value ->
						Value
				end
		end, 
		Dic).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VERIFY IF THE CALLED FUNCTION IS EXPORTED AND GET SPEC %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_exported([],_) -> false;
is_exported([{Name,_}|_],Name) -> true;
is_exported([{_,_}|Rest],Name) ->
	is_exported(Rest,Name).

exported([],_,ExportedTypes) -> 
	ExportedTypes;
exported([Export | Rest], FuncTypes, ExportedTypes) -> 
	FuncName = element(1,Export),
	List1 = lists:filter(fun(X) -> element(1,X) == FuncName end, FuncTypes),
	case List1 of
		[] -> 
			exported(Rest,FuncTypes,ExportedTypes);
		_ -> 
			[FuncType] = List1,
			exported(Rest,FuncTypes,[FuncType | ExportedTypes])
	end.

get_executed_function([],_) -> 
	unexporter;
get_executed_function([{FunName,Arity,Structure,StringSpec}|_],FunName) ->
	{FunName,Arity,Structure,StringSpec};
get_executed_function([_|Functions],FunName) ->
	get_executed_function(Functions,FunName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GET THE TYPES OF THE INPUTS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

type_intersection(Type1,Type2) -> %TODO PENDING
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
		_ -> 
			throw("Non contempled type")
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GET LIST OF PARAMETERS FROM AST %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_parameters(AST,FunName) ->
	{_,_,_,_,Funs,_} = AST,
	{function,_Line,FunName,_Arity,Clauses} = lists:foldl(
		fun (X,Acc) ->
			case X of
				{_,_,FunName,_,_} -> X;
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
join_names_types([],[],Dic) -> 
	Dic;
join_names_types([Name|Names],[Type|Types],Dic) ->
	% printer(Name),
	% printer(Type),
	% io:get_line(""),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE INSTANCES OF THE PARAMETERS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
