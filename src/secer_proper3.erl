-module(secer_proper3).

-export([proper_based/0]).


proper_based() ->
	PoisRels = rele1(),
	ExecFun = funs(),


	% try 
		% register(cuterIn,spawn(secer_trace,init,[])),
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

		{FunName,Arity} = secer_input_gen:divide_function(ExecFun),

		case Arity of
			% 0 ->
			% 	instrument_code(PoisRels,CompareFun),

			% 	ModTmp1 = list_to_atom(filename:basename(FileOld,".erl")++"Tmp"),
			% 	ModTmp2 = list_to_atom(filename:basename(FileNew,".erl")++"Tmp"),

			% 	execute_input(ModTmp1,ModTmp2,FunName,[],CMode);
			_ ->
				% PART 1
				{ParamClauses,TypeDicts} = 
					secer_input_gen:analyze_types(FileOld,FunName,Arity),
				% TimeOut = Time div 3,
				% Inputs = 
				% 	secer_input_gen:execute_cuter(ModuleName1,ModuleName2,FunName,ParamClauses,TypeDicts,TimeOut),
				
				% io:format("~p\n", [{ParamClauses,TypeDicts}]),
				% PART 2
				secer_input_gen:instrument_code(PoisRels,empty),

				% {ok, Fd} = file:open(?TMP_PATH++"cuter.txt", [write]),
				% Self = self(),
				% Ref = make_ref(),

				% spawn(fun() ->
				% 		group_leader(Fd,self()),
				% 		Self ! {cover_compilation(FileOld),Ref}
				% 		end),
				% receive
				% 	{_,Ref} ->
				% 		file:close(Fd)
				% end,

				ModTmp1 = list_to_atom(filename:basename(FileOld,".erl")++"Tmp"),
				ModTmp2 = list_to_atom(filename:basename(FileNew,".erl")++"Tmp"),


				% execute_input(ModTmp1,ModTmp2,FunName,[],CMode),
				FunProper = 
					fun([A, B]) -> 	
						{T1_0, T2_0} = 
							execute_input(ModTmp1,ModTmp2,FunName,[A, B]),
						T1 = 
							lists:map(fun clean_trace/1, T1_0),
						T2 = 
							lists:map(fun clean_trace/1, T2_0),
						{
							'implies', 
							(T1 /= []) andalso (T2 /= []), 
							% true,
							fun() -> 
								% io:format("T1 = ~p\n", [T1]),
								% io:format("T2 = ~p\n", [T2]),
								T1 == T2 
							end
						}
					end,
				TypeA = 
					proper_types:string(),
				TypeB = 
					proper_types:string(),
				% TypeB = 
				% 	proper_types:non_empty(
				% 		proper_types:list(
				% 				proper_types:union(
				% 					[proper_types:exactly(X) || X <- ['0', '1']]))),
				Test = 
					{
						'forall', 
						[TypeA, TypeB], 
						FunProper
					},
				Opts = 
					[
						1000, 
						% quiet, 
						long_result
					],
				Res = 
					proper:quickcheck(Test, Opts),

				io:format("Res: ~p\n", [Res])

				% Opts -> Deberia ser un record

				% Res = 
				% 	proper:shrink([[[4,3],[0,1]]], Test, false_prop, Opts),

				% io:format("Res2: ~p\n", [Res2])

				% Queue = lists:foldl(
				% 	fun(I,L) ->
				% 		RefEx = make_ref(),
				% 		{Trace1,Trace2,Equals} = execute_input(ModTmp1,ModTmp2,FunName,I,CMode),
				% 		input_manager ! {existing_trace,I,{Trace1,Trace2},RefEx,self()},
				% 		receive
				% 			{RefEx,true} ->
				% 				L;
				% 			{RefEx,false} ->
				% 				{Pri,Last} = L,
				% 				case Equals of
				% 					true ->
				% 						{Pri,[I|Last]};
				% 					_ ->
				% 						{[I|Pri],Last}
				% 				end
				% 		end
				% 	end,
				% 	{[],[]},
				% 	Inputs),
				% Input = 
				% 	case Queue of
				% 		{[PH|PT],_} ->
				% 			PH;
				% 		{[],[LH|LT]} ->
				% 			LH
				% 	end,
				% {Clause,Dic} = identify_clause_input(ParamClauses,TypeDicts,Input),
				% validate_input(ModTmp1,ModTmp2,FunName,Queue,Clause,Dic,CMode),
				% gen_random_inputs(ModTmp1,ModTmp2,FunName,ParamClauses,TypeDicts,0,CMode)
		end,
	% catch 
	% 	E:R ->
	% 		%printer({E,R}),
	% 		io:format("ERROR: ~p\n", [{E,R}]),
	% 		{E,R}
	% after
		case whereis(tracer) of
			undefined -> 
				ok;
			_ -> 
				% unregister(cuterIn),
				unregister(tracer)
				% secer ! continue
		end.
	% end.

clean_trace({_,V}) ->
	V.

execute_input(Mod1,Mod2,FunName,Input) ->
	ScValue1 = execute(Mod1,FunName,Input),
	ScValue2 = execute(Mod2,FunName,Input),

	case {ScValue1,ScValue2} of
		{_,timeouted} ->
			% input_manager ! {add,Input,timeouted,Mode},
			{timeouted,timeouted};
		{timeouted,_} ->
			% input_manager ! {add,Input,timeouted,Mode},
			{timeouted,timeouted};
		_ ->
			% RefEq = make_ref(),
			% input_manager ! {add,Input,ScValue1,ScValue2,Mode,self(),RefEq},
			% receive 
			% 	{RefEq,Equals} ->
			% 		Equals
			% end,
			% {ScValue1,ScValue2,Equals}
			{ScValue1,ScValue2}
	end.

execute(MP,Fun,Input) ->
	Res = (catch (apply(MP,Fun,Input))),
	% io:format("Res: ~p\n", [Res]),
	Ref = make_ref(),
	tracer ! {get_results,Ref,self()},
	receive
		{Ref,X} -> X
	end.


% GENERAL

file(o) -> 
	'examples/string/string_old.erl';
file(n) -> 
	'examples/string/string_new.erl';
file(e1) -> 
	'examples/string/string_error1.erl'.
% file(e2) -> 
% 	'examples/happy/happy_error2.erl';
% file(e3) -> 
% 	'examples/happy/happy_error3.erl'.

% POIs

poio() ->
	{file(o), 225, {var, 'Res'}, 1}.

poin() ->
	{file(n), 236, {var, 'Res'}, 1}.

poie1() ->
	{file(e1), 238, {var, 'Res'}, 1}.

poie1_p1() ->
	{file(e1), 233, {var, 'Res1'}, 1}.

poie1_p2() ->
	{file(e1), 236, {var, 'Res2'}, 1}.


rel1() ->
	[{poio(), poin()}].

rele1() ->
	[{poio(), poie1()}].

rele1_1() ->
	[{poio(), poie1_p1()}].

rele1_2() ->
	[{poio(), poie1_p2()}].

% rele1() ->
% 	[{poio(), poie1()}].

% rele2() ->
% 	[{poio(), poie2()}].

% rele3() ->
% 	[{poio(), poie2()}].

% rel2() ->
% 	[{poio_2(), poin_2()}].

% rele1_2() ->
% 	[{poio_2(), poie1_2()}].

% rele2_2() ->
% 	[{poio_2(), poie2_2()}].

% rele3_2() ->
% 	[{poio_2(), poie3_2()}].

funs() ->
	"tokens/2".