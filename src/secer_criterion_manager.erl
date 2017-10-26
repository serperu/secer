-module(secer_criterion_manager).
-export([main/2]). 
-define(TMP_PATH,"./tmp/").

-record(nodeinfo, {id,env,bound,free}).


main(POIList,CompareFun) ->

		%START
		NewPoiList = poi_transformation(POIList),
		{PoiListOld,PoiListNew} = divide_poi_list(NewPoiList),
		FileOld = atom_to_list(element(1,lists:nth(1,PoiListOld))),
		FileNew = atom_to_list(element(1,lists:nth(1,PoiListNew))),

		%OLD FILE
		{LineColPOIsOld,ExplicitPOIsOld} = lists:foldl(
			fun(P,{LCP,EP}) ->
				case P of
					{_,{_,_},{_,_}} ->
						{[P|LCP],EP};
					_ ->
						{LCP,[P|EP]}
				end
			end,	
			{[],[]},
			PoiListOld),

		{ok,ASTOld} = epp:parse_file(FileOld,[],[]),

		%ANOTAR TODOS LOS FORMS
		{AnnASTOld,LastId} = lists:mapfoldl(fun annotate/2,1,ASTOld),

		%LINE,TYPE,OC POIS
		LTOPoisOld = [{catch lists:foldl(fun find_explicit_pois/2,POI,AnnASTOld),POI} || POI <- ExplicitPOIsOld],

		%LINE COL POIS
		LCPoisOld = [
			begin 
				{_,Program} = read_expression(POI),
				{_,{SLine,_},_} = POI,
				{ok,ChangedFd} = file:open(?TMP_PATH++FileOld,[write,{encoding, unicode}]),
				io:format(ChangedFd,"~s",[Program]),
				{ok,AST0} = epp:parse_file(?TMP_PATH++FileOld,[],[]),
				{{FunName,FunArity},Path} = (catch lists:foldl(fun find_line_col_pois/2,{SLine,poi_ref},AST0)),
				{get_matching_id(FunName,FunArity,Path,AnnASTOld),POI} 
			end || POI <- LineColPOIsOld],

		ListOld = LTOPoisOld ++ LCPoisOld,
		
		OrderIdPoiListOld = lists:sort(fun({A,_}, {B,_}) -> A =< B end, ListOld),
		IdPoiDict = dict:from_list(OrderIdPoiListOld),

		get_replaced_AST(FileOld,OrderIdPoiListOld,AnnASTOld),

		%NEW FILE
		{LineColPOIsNew,ExplicitPOIsNew} = lists:foldl(
			fun(P,{LCP,EP}) ->
				case P of
					{_,{_,_},{_,_}} ->
						{[P|LCP],EP};
					_ ->
						{LCP,[P|EP]}
				end
			end,	
			{[],[]},
			PoiListNew),

		{ok,ASTNew} = epp:parse_file(FileNew,[],[]),

		%ANOTAR TODOS LOS FORMS
		{AnnASTNew,_} = lists:mapfoldl(fun annotate/2,LastId,ASTNew),

		%LINE,TYPE,OC POIS
		LTOPoisNew = [{catch lists:foldl(fun find_explicit_pois/2,POI,AnnASTNew),POI} || POI <- ExplicitPOIsNew],

		LCPoisNew = [
			begin 
				{_,Program} = read_expression(POI),
				{_,{SLine,_},_} = POI,
				{ok,ChangedFd} = file:open(?TMP_PATH++FileNew,[write,{encoding, unicode}]),
				io:format(ChangedFd,"~s",[Program]),
				{ok,AST0} = epp:parse_file(?TMP_PATH++FileNew,[],[]),
				{{FunName,FunArity},Path} = (catch lists:foldl(fun find_line_col_pois/2,{SLine,poi_ref},AST0)),
				{get_matching_id(FunName,FunArity,Path,AnnASTNew),POI} 
			end || POI <- LineColPOIsNew],

		ListNew = LTOPoisNew ++ LCPoisNew,
		OrderIdPoiListNew = lists:sort(fun({A,_}, {B,_}) -> A =< B end, ListNew),
		FinalIdPoiDict = dict:merge(fun(_,_,V2) -> V2 end,IdPoiDict,dict:from_list(OrderIdPoiListNew)),
		
		get_replaced_AST(FileNew,OrderIdPoiListNew,AnnASTNew),

		input_manager ! {set_rels,NewPoiList,FinalIdPoiDict},
		case CompareFun of
			empty ->
				ok;
			_ ->
				input_manager ! {set_fun,CompareFun}
		end.

%%%%%%%%%%%%%%%%%%%%%
%%% LINE COL POIS %%%
%%%%%%%%%%%%%%%%%%%%%
read_expression(Poi) ->
	{FileI,{LineI,ColI},{LineF,ColF}} = Poi,
	{ok,FileContent} = file:read_file(FileI),
	FileString = unicode:characters_to_list(FileContent),
	Tokens = re:split(FileString,"\n",[{return,list}]),

	%Expr Lines
	SelLines = LineF+1 - LineI,
	Sublist = lists:sublist(Tokens,LineI,SelLines),
	[L1|T] = Sublist,
	L1Replaced = string:substr(L1,ColI),
	PoiExpr = case LineI of
		LineF ->
			string:substr(L1Replaced,1,ColF+1 - ColI);
		_ ->
			ModList = modifyLastElem(T,ColF+1,[]),
			lists:foldr(
				fun(L,S) ->
					lists:concat([S,"\n",L])
				end,
			L1Replaced,
			ModList)
	end,

	%Changed Program
	RemainingHead = lists:sublist(Tokens,1,LineI-1),
	Replaced = case LineI of 
		LineF ->
			ModLine = lists:nth(LineI,Tokens),
			Begin = string:substr(ModLine,1,ColI-1),
			End = string:substr(ModLine,ColF+1,length(ModLine)-ColF),
			[Begin++"poi_ref"++End];
		_ ->
			[FirstLine|Rest] = lists:sublist(Tokens,LineI,LineF-LineI+1),
			Begin = string:substr(FirstLine,1,ColI-1),

			Last = lists:last(Rest),
			End = string:substr(Last,ColF+1,length(Last)-ColF),
			[Begin++"poi_ref"++End]
	end,
	RemainingTail = lists:sublist(Tokens,LineF+1,length(Tokens)-LineF),
	[First|OtherLines] = lists:append([RemainingHead,Replaced,RemainingTail]),

	ChangedProgram = lists:foldl(
		fun(L,S) ->
			lists:concat([S,"\n",L])
		end,
	First,
	OtherLines),
	{PoiExpr,ChangedProgram}.

modifyLastElem([L],Col,NewL) ->
	[string:substr(L,1,Col-1)|NewL];
modifyLastElem([H|T],Col,NewL) ->
	modifyLastElem(T,Col,[H|NewL]).

find_line_col_pois(Root,{Line,PoiName}) -> 
	try
		case erl_syntax:type(Root) of
			function ->				
				get_path_aux(Root,Line,PoiName),
				{Line,PoiName};
			_ ->
				{Line,PoiName}
		end
	catch
		Path -> 
			throw({{erl_syntax:atom_value(erl_syntax:function_name(Root)),erl_syntax:function_arity(Root)},
					lists:reverse(Path)})
	end.

get_path_aux(Root,Line,PoiName) ->
	list_of_children(erl_syntax:subtrees(Root),Line,PoiName,[]),
	unfound.
list_of_children(L,Line,PoiName,Path) ->
	lists:foldl(
		fun(E, {Li,Poi,NAcc}) ->
			child_treatment(E,{Li,Poi,NAcc},Path),
			{Li,Poi,NAcc + 1}
		end,
		{Line,PoiName,1},
		L).
child_treatment(L,{Line,PoiName,N},Path) ->
	lists:foldl(
		fun(E, {Li,Poi,MAcc}) ->
			New_path = [{erl_syntax:type(E),N,MAcc}|Path],
			case E of
				{atom,Li,Poi} -> 
					throw(New_path);
				_ ->
					list_of_children(erl_syntax:subtrees(E),Li,Poi,New_path),
					{Li,Poi,MAcc+1}
			end
		end,
		{Line,PoiName,1},
		L).

get_matching_id(FunName,FunArity,Path,AST) ->
	catch lists:foldl(
		fun(Node,{N,A,P}) -> 
			case erl_syntax:type(Node) of
				function ->
					Name = erl_syntax:atom_value(erl_syntax:function_name(Node)),
					Arity = erl_syntax:function_arity(Node),
					case {Name,Arity} of
						{N,A} ->
							Poi = obtain_sc(Node,P),
							[Ann] = erl_syntax:get_ann(Poi),
							throw(Ann#nodeinfo.id);
						_ ->
							{N,A,P}
					end;	
				_ ->
					{N,A,P}
			end
		end,
		{FunName,FunArity,Path},
		AST).

%%%%%%%%%%%%%%%%%%%%
%%% ANNOTATE AST %%%
%%%%%%%%%%%%%%%%%%%%
annotate(Node,CurrentId) ->
	AnnAST = erl_syntax_lib:annotate_bindings(Node,ordsets:new()),
	erl_syntax_lib:mapfold(
		fun(N,Id) ->
			[Env,Bound,Free] = erl_syntax:get_ann(N),
			Ann = #nodeinfo{id = Id,env = Env,bound = Bound,free = Free},
			{erl_syntax:set_ann(N,[Ann]),Id+1}
		end,
		CurrentId,
		AnnAST).

%%%%%%%%%%%%%%%%%%%%%
%%% EXPLICIT POIS %%%
%%%%%%%%%%%%%%%%%%%%%
poi_transformation(PoiList) ->
	lists:map(
		fun({Poi1,Poi2}) ->
			NPoi1 = poi_map(Poi1),
			NPoi2 = poi_map(Poi2),
			{NPoi1,NPoi2}
		end,
		PoiList).

poi_map(Poi) ->
	case Poi of
		{F,L,'if',O} ->
			{F,L,if_expr,O};
		{F,L,'case',O} ->
			{F,L,case_expr,O};
		{F,L,call,O} ->
			{F,L,application,O};
		_ ->
			Poi
	end.

find_explicit_pois(Root,{File,Line,{Type,Poi_Name},Oc}) ->
	try 
		case erl_syntax:type(Root) of
			function ->
				children_list(erl_syntax:subtrees(Root),Line,Poi_Name,Type,Oc,1),
				{File,Line,{Type,Poi_Name},Oc};
			_ ->
				{File,Line,{Type,Poi_Name},Oc}
		end
	catch
		[Ann] -> 
			throw(Ann#nodeinfo.id)
	end;
find_explicit_pois(Root,{File,Line,Type,Oc}) ->
	try 
		case erl_syntax:type(Root) of
			function ->
				children_list(erl_syntax:subtrees(Root),Line,null,Type,Oc,1),
				{File,Line,Type,Oc};
			_ ->
				{File,Line,Type,Oc}
		end
	catch
		[Ann] -> 
			throw(Ann#nodeinfo.id)
	end.

children_list(L,Line,Poi_Name,Type,Oc,CurrentOc) ->
	lists:foldl(
		fun(E, {Li,Poi,T,O,CurOc}) ->
			{_,_,_,_,NewCurrentOc} = child(E,{Li,Poi,T,O,CurOc}),
			{Li,Poi,T,O,NewCurrentOc}
		end,
		{Line,Poi_Name,Type,Oc,CurrentOc},
		L).

child(L,{Line,null,ExprType,Oc,CurrentOc}) ->
	lists:foldl(
		fun(E,{Li,Poi,T,O,CurOc}) ->
			{_,Type,{_,IfLine,_,_},_} = E,
			case {Type,IfLine} of
				{T,Li} when O == CurOc -> 
					throw(erl_syntax:get_ann(E));
				{T,Li} ->
					{_,_,_,_,NewCurrentOc} = children_list(erl_syntax:subtrees(E),Li,Poi,T,O,CurOc),
					{Li,Poi,T,O,NewCurrentOc+1};
				_ ->
					{_,_,_,_,NewCurrentOc} =children_list(erl_syntax:subtrees(E),Li,Poi,T,O,CurOc),
					{Li,Poi,T,O,NewCurrentOc}
			end
		end,
		{Line,null,ExprType,Oc,CurrentOc},
		L);
child(L,{Line,Poi_Name,ExprType,Oc,CurrentOc}) ->
	lists:foldl(
		fun(E,{Li,Poi,T,O,CurOc}) ->
			{_,_,_,ComparableE} = E,
			case ComparableE of
				{T,Li,Poi} when O == CurOc -> 
					throw(erl_syntax:get_ann(E));
				{T,Li,Poi} ->
					{_,_,_,_,NewCurrentOc} = children_list(erl_syntax:subtrees(E),Li,Poi,T,O,CurOc),
					{Li,Poi,T,O,NewCurrentOc+1};
				_ ->
					{_,_,_,_,NewCurrentOc} =children_list(erl_syntax:subtrees(E),Li,Poi,T,O,CurOc),
					{Li,Poi,T,O,NewCurrentOc}
			end
		end,
		{Line,Poi_Name,ExprType,Oc,CurrentOc},
		L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INSTRUMENTATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_replaced_AST(File,PoiList,AST) -> 
	ModuleName = filename:basename(File,".erl"),
	{_,Final_AST} = lists:mapfoldl(fun instrument_poi/2,AST,PoiList),
	
	{ok,Final_file} = file:open(?TMP_PATH++ModuleName++"Tmp.erl",[write]),
	generate_final_code(Final_AST,Final_file),

	compile:file(?TMP_PATH++ModuleName++"Tmp.erl",[{outdir,?TMP_PATH}]),
	code:purge(list_to_atom(ModuleName++"Tmp")),
	code:load_abs(?TMP_PATH++ModuleName++"Tmp").

instrument_poi(Poi,AST) ->
	{PoiId,_} = Poi,
	{Poi,instrument_AST(AST,PoiId)}.

generate_final_code(AST,File) ->
	lists:mapfoldl(fun revert_code/2,File,AST).

revert_code(Form,File) ->
	case erl_syntax:type(Form) of
		attribute ->
			Attr_name = erl_syntax:attribute_name(Form),
			case {erl_syntax:is_atom(Attr_name,file),erl_syntax:is_atom(Attr_name,module)} of
				{true,_} ->
					{empty,File};
				{_,true} ->
					{ok,Filename} = file:pid2name(File),
					ModuleName = filename:basename(Filename,".erl"),
					New_module = erl_syntax:attribute(Attr_name,[erl_syntax:atom(ModuleName)]),
					{io:format(File,"~s",[erl_pp:form(erl_syntax:revert(New_module))]),File};
				_ -> 
					{io:format(File,"~s",[erl_pp:form(erl_syntax:revert(Form))]),File}
			end;
		_ ->
			{io:format(File,"~s",[erl_pp:form(erl_syntax:revert(Form))]),File}
	end.

instrument_AST(AST,PoiId) ->
	{New_AST,{_,Found}} = lists:mapfoldl(fun map_instrument_AST/2,{PoiId,false},AST),
	case Found of
		true ->
			ok;
		false ->
			%io:format("No variable ~s occurrence ~p found in line ~p\n",[PoiName,Oc,Line]),
			io:format("The POI:~p is not present in the indicated location\n",[PoiId]),
			secer ! die,
			exit(0)
	end,
	New_AST.

map_instrument_AST(Node,{PoiId,Found}) ->
	case Found of
		true ->
			{Node,{PoiId,Found}};
		_ ->
			case erl_syntax:type(Node) of
				function ->
					SC_path = get_path(Node,PoiId),
					case SC_path of 
						unfound -> 
							{Node,{PoiId,Found}};
						_ -> 
							%Ann_node = erl_syntax_lib:annotate_bindings(Node,ordsets:new()),
							% POSIBLEMENTE ESTO SE PUEDE SACAR FUERA (Se ejecuta en cada POI)
							Var_list = sets:to_list(erl_syntax_lib:variables(Node)),
							add_vars_to_var_gen(Var_list),
							{instrument(Node,SC_path),{PoiId,true}}
					end;
				_ -> 
					{Node,{PoiId,Found}}
			end
	end.

%%%%%%%%%%%%%%%%
%%% GET PATH %%%
%%%%%%%%%%%%%%%%
get_path(Root,PoiId) ->
	try 
		list_of_lists(erl_syntax:subtrees(Root),PoiId,[]),
		unfound
	catch
		Path -> Path
	end.
list_of_lists(L,PoiId,Path) ->
	lists:foldl(
		fun(E, {PId,NAcc}) ->
			list(E,{PId,NAcc},1,Path),
			{PId,NAcc + 1}
		end,
		{PoiId,1},
		L).
list(L,{PoiId,N},_,Path) ->
	lists:foldl(
		fun(E, {PId,MAcc}) ->
			New_path = [{erl_syntax:type(E),N,MAcc}|Path],
			Ann = erl_syntax:get_ann(E),
			case Ann of
				[Info] ->
					NodeId = Info#nodeinfo.id,
					case NodeId of
						PId ->
							throw(New_path);
						_ ->
							list_of_lists(erl_syntax:subtrees(E),PId,New_path),
							{PId,MAcc+1}
					end;
				[] ->
					list_of_lists(erl_syntax:subtrees(E),PId,New_path),
					{PId,MAcc+1}
			end
		end,
		{PoiId,1},
		L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ADD VARS TO VAR GEN %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_vars_to_var_gen([]) ->
	var_gen ! all_variables_added;
add_vars_to_var_gen([Var|Vars]) ->
	var_gen ! {add_variable,atom_to_list(Var)},
	add_vars_to_var_gen(Vars).

%%%%%%%%%%%%%%%%%%
%%% INSTRUMENT %%%
%%%%%%%%%%%%%%%%%%
instrument(Node,Path) -> 
	{Root_to_node,Node_to_sc} = divide_path(Path,[]),
	Instrumented_AST = sc_replacer(Node,{Root_to_node,Node_to_sc}),
	Instrumented_AST.

divide_path([],L2) ->
	{[],L2};
divide_path([Node|Father],[]) ->
	divide_path(Father,[Node]);
divide_path([{clause,N1,M1}|Father],[{Type,1,M2}|T]) -> % PATTERN
	divide_path(Father,[{clause,N1,M1}|[{Type,1,M2}|T]]);
divide_path([{clause,N1,M1}|Father],[{Type,2,M2}|T]) -> % GUARD
	divide_path(Father,[{clause,N1,M1}|[{Type,2,M2}|T]]);
divide_path([Node|Father],L2) ->
 	case Node of
 		{match_expr,_,_} -> 
 			{lists:reverse([Node|Father]),L2};
 		{clause,_,_} -> 
 			{lists:reverse([Node|Father]),L2};
 		{list_comp,_,_} -> 
 			{lists:reverse([Node|Father]),L2};
 		{case_expr,_,_} -> 
 			{lists:reverse([Node|Father]),L2};
 		{try_expr,_,_} ->
 			{lists:reverse([Node|Father]),L2};
 		{receive_expr,_,_} ->
 			{lists:reverse([Node|Father]),L2};
 		{if_expr,_,_} ->
 			{lists:reverse([Node|Father]),L2};
 		{_,_,_} ->
 			divide_path(Father,[Node|L2])
 	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CASE WITH THE INSTRUMENTED EXPRESSIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sc_replacer(Node,{[],Node_to_sc}) ->
	replace_expression_with_clauses(Node,Node_to_sc);
sc_replacer(Node,{[{Type,N,M}],Node_to_sc}) ->
	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N,Children),
	Elem = lists:nth(M,Child),

	New_elem = 	case Type of
					match_expr ->
						replace_match(Elem,Node_to_sc);
					list_comp ->
						replace_lc(Elem,Node_to_sc);
					_ ->
						replace_expression_with_clauses(Elem,Node_to_sc)
				end,

	New_child = replacenth(M,New_elem,Child),
	New_children = replacenth(N,New_child,Children),
	replace_node_with_anns(Node,New_children);

	%erl_syntax:make_tree(erl_syntax:type(Node),New_children);

sc_replacer(Node,{[{_,N,M}|T],Node_to_sc}) ->
	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N,Children),
	Elem = lists:nth(M,Child),

	New_elem = sc_replacer(Elem,{T,Node_to_sc}),

	New_child = replacenth(M,New_elem,Child),
	New_children = replacenth(N,New_child,Children),
	replace_node_with_anns(Node,New_children).
	%erl_syntax:make_tree(erl_syntax:type(Node),New_children).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% REPLACE SC IN DIFFERENT STRUCTURES %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%
% MATCHES %
%%%%%%%%%%%	
replace_match(Node,[{Type,N,M}|T]) ->
	case N of
		1 -> 
			replace_match_pattern(Node,[{Type,N,M}|T]);
		_ ->
			replace_expression(Node,[{Type,N,M}|T])
	end.

replace_match_pattern(Node,[{Type,N,M}|T]) -> 
	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N,Children),

	{[New_pattern],Var_sc_fv} = replace_pattern_with_free_variables(Child,[{Type,N,M}|T],dict:new()),

	Expr_pm = erl_syntax:match_expr(New_pattern,erl_syntax:match_expr_body(Node)),
	
	%Var_sc_fv = erlang:get(slicing_criterion),
	Node_sc = obtain_sc(Node,[{Type,N,M}|T]),

	[PoiAnn] = erl_syntax:get_ann(Node_sc),
	PoiId = PoiAnn#nodeinfo.id,

	Ann = erl_syntax:get_ann(Node),
	Bounded_vars =
		case Ann of
			[Info] ->
				{free,B} = Info#nodeinfo.free,
				B;
			[] ->
				[]
		end,	

	PoiName = erl_syntax:variable_name(Node_sc),

	Expr_block = case lists:member(PoiName,Bounded_vars) of
		true ->	
			Expr_send_sc = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
							erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),Node_sc])])),
			erl_syntax:block_expr([Expr_pm,Expr_send_sc,New_pattern]);
		false ->
			Expr_send_fv = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
							erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),Var_sc_fv])])),
			erl_syntax:block_expr([Expr_pm,Expr_send_fv,New_pattern])
	end,
	erl_syntax:match_expr(erl_syntax:match_expr_pattern(Node),Expr_block).

%%%%%%%%%%%%%%%%%%%%%%%
% LIST COMPREHENSIONS %
%%%%%%%%%%%%%%%%%%%%%%%
replace_lc(Node,[{generator,N1,M1},{Type,N2,M2}|T]) ->
	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N1,Children),
	Elem = lists:nth(M1,Child),

	New_generator = replace_generator(Elem,[{Type,N2,M2}|T]),

	Final_child = case N2 of 
		1 -> 
			New_child = replacenth(M1,New_generator,Child),
			New_generator_aux = add_neccessary_generator(Elem,[{Type,N2,M2}|T],erl_syntax:generator_pattern(New_generator)),
			add_at_nth(New_generator_aux,M1+1,1,New_child,[]);
		_ -> 
			replacenth(M1,New_generator,Child)
	end,

	New_children = replacenth(N1,Final_child,Children),
	replace_node_with_anns(Node,New_children);
	%erl_syntax:make_tree(erl_syntax:type(Node),New_children);

replace_lc(Node,Path) -> 
 	replace_expression(Node,Path).

replace_generator(Node,[{Type,N,M}|T]) ->
	case N of
		1 -> % GENERATOR PATTERN
			[New_pattern] = replace_novar_pattern_with_free_variables([erl_syntax:generator_pattern(Node)],[{Type,N,M}|T]),
			erl_syntax:generator(New_pattern,erl_syntax:generator_body(Node));
		_ ->
			replace_expression(Node,[{Type,N,M}|T])
	end.

add_neccessary_generator(Node,[{Type,N,M}|T],New_pattern) ->
	Old_pattern = erl_syntax:generator_pattern(Node),
	case N of
		1 ->
			Node_sc = obtain_sc(Node,[{Type,N,M}|T]),
			[PoiAnn] = erl_syntax:get_ann(Node_sc),
			PoiId = PoiAnn#nodeinfo.id,

			Expr_send_sc = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
							erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),Node_sc])])),
			Expr_list_gen = erl_syntax:list([New_pattern]),
			Gen_Body = erl_syntax:block_expr([Expr_send_sc,Expr_list_gen]),

			erl_syntax:generator(Old_pattern,Gen_Body);
		 _ -> 
		 	throw("ERROR ADDING GENERATOR IN LC")
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPRESSIONS WITH CLAUSES %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CASE, IF, FUNCTION, TRYOF-CATCH, RECEIVE, GUARDS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_expression_with_clauses(Node,[{clause,N1,M1},{Type,N2,M2}|T]) ->
	Node_type = erl_syntax:type(Node),

	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N1,Children),
	Elem = lists:nth(M1,Child),

	Clauses = get_node_clauses(Node,N1,M1),

	New_clause = case {Type,N2} of
		{disjunction,_} -> 
			replace_guard(Elem,[{Type,N2,M2}|T],Clauses,Node_type);
		{_,1} -> 
			replace_clause(Elem,[{Type,N2,M2}|T],Clauses,Node_type);
		_ ->
			replace_expression(Elem,[{Type,N2,M2}|T])
	end,
	
	New_child = generate_new_child(Node_type,M1,New_clause,Child),
	New_children = replacenth(N1,New_child,Children),
	replace_node_with_anns(Node,New_children);
	%erl_syntax:make_tree(Node_type,New_children);
replace_expression_with_clauses(Node,[{Type,N,M}|T]) ->
	replace_expression(Node,[{Type,N,M}|T]).

get_node_clauses(Node,N,M) -> 
	case erl_syntax:type(Node) of
		function ->
			Clauses = erl_syntax:function_clauses(Node),
			Lasts = lists:nthtail(M-1,Clauses),
			adapt_function_patterns(Lasts,[]);
		case_expr ->	
			Clauses = erl_syntax:case_expr_clauses(Node),
			lists:nthtail(M-1,Clauses);
		receive_expr ->
			Clauses = erl_syntax:receive_expr_clauses(Node),
			lists:nthtail(M-1,Clauses);
		try_expr ->
			Clauses = case N of
				2 -> erl_syntax:try_expr_clauses(Node);
				3 -> erl_syntax:try_expr_handlers(Node)
			end,
			lists:nthtail(M-1,Clauses);
		if_expr ->
			Clauses = erl_syntax:if_expr_clauses(Node),
			Lasts = lists:nthtail(M-1,Clauses),
			add_pattern(Lasts,[]);
		_ ->
			throw("Uncontempled type of node")
	end.

adapt_function_patterns([],New_clauses) -> 
	lists:reverse(New_clauses);
adapt_function_patterns([Clause|Clauses],New_clauses) ->
	Old_pattern = erl_syntax:clause_patterns(Clause),
	New_clause = erl_syntax:clause([erl_syntax:tuple(Old_pattern)],erl_syntax:clause_guard(Clause),erl_syntax:clause_body(Clause)),
	adapt_function_patterns(Clauses,[New_clause|New_clauses]).

add_pattern([],New_clauses) -> 
	lists:reverse(New_clauses);
add_pattern([Clause|Rest],New_clauses) ->
	New_clause = erl_syntax:clause([erl_syntax:underscore()],erl_syntax:clause_guard(Clause),erl_syntax:clause_body(Clause)),
	add_pattern(Rest,[New_clause|New_clauses]).

generate_new_child(Type,M,New_clause,Child) ->
	case Type of
		if_expr -> 
			{New_clauses,_} = lists:split(M,Child),
			replacenth(M,New_clause,New_clauses);
		_ -> 
			replacenth(M,New_clause,Child)
	end.

%%%%%%%%%%
% GUARDS %
%%%%%%%%%%
% <=== TODO ===>
% ESTA FUNCION RECOGE EL VALOR DE TODAS LAS VARIABLES DE LA GUARDA SE EVALUEN 
% O NO (IGNORA CORTOCIRCUITADOS)
% <============>
replace_guard(Node,Path,Clauses,Root_type) ->	
	Node_sc = obtain_sc(Node,Path),
	[PoiAnn] = erl_syntax:get_ann(Node_sc),
	PoiId = PoiAnn#nodeinfo.id,

	Expr_send_sc = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
							erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),Node_sc])])),
	
	Pattern = erl_syntax:clause_patterns(Node),
	Expr_case = generate_case_expression(Pattern,Clauses,Root_type), 

	Expr_block = erl_syntax:block_expr([Expr_send_sc,Expr_case]),
	erl_syntax:clause(erl_syntax:clause_patterns(Node),[],[Expr_block]).

%%%%%%%%%%%
% CLAUSES %
%%%%%%%%%%%
replace_clause(Node,[{Type,N,M}|T],Clauses,Root_type) ->
	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N,Children),

	%REPLACE PATTERN
	{New_pattern,Var_sc_fv} = replace_pattern_with_free_variables(Child,[{Type,N,M}|T],dict:new()),

	%CREATE BODY
	%Var_sc_fv = erlang:get(slicing_criterion),
	Node_sc = obtain_sc(Node,[{Type,N,M}|T]),
	[PoiAnn] = erl_syntax:get_ann(Node_sc),
	PoiId = PoiAnn#nodeinfo.id,

	Ann = erl_syntax:get_ann(Node),
	Bounded_vars =
		case Ann of
			[Info] ->
				{free,B} = Info#nodeinfo.free,
				B;
			[] ->
				[]
		end,	

	PoiName = erl_syntax:variable_name(Node_sc),

	Expr_block = case lists:member(PoiName,Bounded_vars) of
		true ->	
			Expr_send_fv = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
							erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),Var_sc_fv])])),
			Expr_send_sc = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
							erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),Node_sc])])),
			Case_clause_equal = erl_syntax:clause([Node_sc],[],[Expr_send_fv]),
			Case_clause_else = erl_syntax:clause([erl_syntax:underscore()],[],[Expr_send_sc]),
			Expr_tracer_case = erl_syntax:case_expr(Var_sc_fv,[Case_clause_equal,Case_clause_else]),
			
			Expr_clauses_case = generate_case_expression(New_pattern,Clauses,Root_type),
			erl_syntax:block_expr([Expr_tracer_case,Expr_clauses_case]);
		_ ->
			Expr_send_fv = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
							erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),Var_sc_fv])])),
			Expr_case = generate_case_expression(New_pattern,Clauses,Root_type),
			erl_syntax:block_expr([Expr_send_fv,Expr_case])
	end,

	%CREATE CLAUSE
	erl_syntax:clause(New_pattern,[],[Expr_block]).

generate_case_expression(Pattern,Clauses,Type) ->
	case Type of
		function -> 
			erl_syntax:case_expr(erl_syntax:tuple(Pattern),Clauses);
		if_expr ->
			erl_syntax:case_expr(erl_syntax:atom("empty_expression"),Clauses);
		try_expr ->
			[Pattern0] = Pattern,
			Is_pattern_class_qualifier = erl_syntax:type(Pattern0) == class_qualifier,
			Are_catch_patterns = lists:any(
									fun(Elem) -> 
										[Clause_pattern] = erl_syntax:clause_patterns(Elem),
										erl_syntax:type(Clause_pattern) == class_qualifier
									end,
									Clauses),
			Are_special_patterns = Is_pattern_class_qualifier or Are_catch_patterns,
			{New_pattern,New_clauses} = case Are_special_patterns of
				true -> 
					{generate_new_catch_pattern(Pattern),generate_new_catch_clauses(Clauses,[])};
				false ->
					{Pattern0,Clauses}
			end,
			erl_syntax:case_expr(New_pattern,New_clauses);
			
		_ -> 
			[New_pattern] = Pattern,
			erl_syntax:case_expr(New_pattern,Clauses)
	end.

generate_new_catch_pattern([Pattern]) ->
	case erl_syntax:type(Pattern) of
		class_qualifier ->
			Elem1 = erl_syntax:class_qualifier_argument(Pattern),
			Elem2 = erl_syntax:class_qualifier_body(Pattern),
			erl_syntax:tuple([Elem1,Elem2]);
		_ ->
			Elem1 = erl_syntax:underscore(),
			erl_syntax:tuple([Elem1,Pattern])
	end.

generate_new_catch_clauses([],New_clauses) ->
	lists:reverse(New_clauses);
generate_new_catch_clauses([Clause|Clauses],New_clauses) ->
	[Pattern] = erl_syntax:clause_patterns(Clause),
	New_pattern = case erl_syntax:type(Pattern) of
		class_qualifier ->
			Elem1 = erl_syntax:class_qualifier_argument(Pattern),
			Elem2 = erl_syntax:class_qualifier_body(Pattern),
			erl_syntax:tuple([Elem1,Elem2]);
		_ ->
			Elem1 = erl_syntax:underscore(),
			erl_syntax:tuple([Elem1,Pattern])
	end,

	New_clause = erl_syntax:clause([New_pattern],erl_syntax:clause_guard(Clause),erl_syntax:clause_body(Clause)),
	generate_new_catch_clauses(Clauses,[New_clause|New_clauses]).

%%%%%%%%%%%%%%%
% EXPRESSIONS %
%%%%%%%%%%%%%%%
replace_expression(Node,[{_,N,M}|T]) ->
	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N,Children),
	Elem = lists:nth(M,Child),

	[PoiAnn] = erl_syntax:get_ann(Elem),
	PoiId = PoiAnn#nodeinfo.id,


	Replaced_expression = case T of
		[] ->
			PatternFV = gen_free_var(),
			Expr_pm = erl_syntax:match_expr(PatternFV,Elem),
			Expr_send = erl_syntax:infix_expr(erl_syntax:atom("tracer"),erl_syntax:operator("!"),
						 erl_syntax:tuple([erl_syntax:atom(add),erl_syntax:tuple([erl_syntax:integer(PoiId),PatternFV])])),
			erl_syntax:block_expr([Expr_pm,Expr_send,PatternFV]);
		_ ->
			replace_expression(Elem,T)
	end,
	
	New_child = replacenth(M,Replaced_expression,Child),
	New_children = replacenth(N,New_child,Children),
	replace_node_with_anns(Node,New_children).
	%erl_syntax:make_tree(erl_syntax:type(Node),New_children).

%%%%%%%%%%
% COMMON %
%%%%%%%%%%
replace_pattern_with_free_variables(Pattern,[{_,_,M}],VarDic) -> % DEVUELVE EN FORMATO [Pattern1,Pattern2...]
	{Modified_pattern,_} = replace_after_position(Pattern,M,VarDic),
	Sc_fv = gen_and_put_scFreeVar(),
	{replacenth(M,Sc_fv,Modified_pattern),Sc_fv};
replace_pattern_with_free_variables(Pattern,[{_Type1,_N1,M1},{_Type,N,M2}|T],VarDic) ->
	{New_pattern,NewDic} = replace_after_position(Pattern,M1,VarDic),
	
	Sc_elem = lists:nth(M1,New_pattern),
	Children = erl_syntax:subtrees(Sc_elem),
	Child = lists:nth(N,Children),

	{New_child,Sc_fv} = replace_pattern_with_free_variables(Child,[{_Type,N,M2}|T],NewDic),
	
	Final_children = replacenth(N,New_child,Children),
	{replacenth(M1,erl_syntax:make_tree(erl_syntax:type(Sc_elem),Final_children),New_pattern),Sc_fv}.

gen_and_put_scFreeVar() ->
	Ref = make_ref(),
	var_gen ! {get_free_variable,Ref,self()},
	New_var = receive
				{FV,Ref} -> 
					FV
			  end,
	%erlang:put(slicing_criterion,New_var),
	New_var.

obtain_sc(Node,[]) -> 
	Node;
obtain_sc(Node,[{_,N,M}|T]) ->
	Children = erl_syntax:subtrees(Node),
	Child = lists:nth(N,Children),
	Elem = lists:nth(M,Child),
	obtain_sc(Elem,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% REPLACE PATTERN LC %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_novar_pattern_with_free_variables(Pattern,[{_,_,M}]) -> % DEVUELVE EN FORMATO [Pattern1,Pattern2...]
	New_Pattern = replace_all_after_position(Pattern,M),
	NewSC = gen_and_put_scFreeVar(),
	replacenth(M,NewSC,New_Pattern);
replace_novar_pattern_with_free_variables(Pattern,[{_,_,M1},{_Type,N,M2}|T]) ->
	New_pattern = replace_all_after_position(Pattern,M1),

	Sc_elem = lists:nth(M1,New_pattern),
	Children = erl_syntax:subtrees(Sc_elem),
	Child = lists:nth(N,Children),

	New_child = replace_novar_pattern_with_free_variables(Child,[{_Type,N,M2}|T]),

	Final_children = replacenth(N,New_child,Children),
	replacenth(M1,erl_syntax:make_tree(erl_syntax:type(Sc_elem),Final_children),New_pattern). % CAMBIAR TAMBIEN EN EL OTRO REPLACE PATTERN

replace_all_after_position(List,Index) ->
	replace_all_after_position(List,Index,[],1).

replace_all_after_position([],_,New_list,_) ->
	lists:reverse(New_list);
replace_all_after_position([H|T],Index,New_list,Pos) ->
	case Pos > Index of
		true ->
			New_H = gen_free_var(),
			replace_all_after_position(T,Index,[New_H|New_list],Pos+1);
		false ->
			replace_all_after_position(T,Index,[H|New_list],Pos+1)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISCELANEA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
divide_poi_list(List) ->
	divide_poi_list(List,[],[]).

divide_poi_list([],OldList,NewList) ->
	{lists:reverse(OldList),lists:reverse(NewList)};
divide_poi_list([{Old,New}|Tail],OldList,NewList) ->
	NewOld = case lists:member(Old,OldList) of
		true ->
			OldList;
		false ->
			[Old|OldList]
	end,
	NewNew = case lists:member(New,NewList) of
		true ->
			NewList;
		false ->
			[New|NewList]
	end,
	divide_poi_list(Tail,NewOld,NewNew).
%%%%%%%%%%%%%%%%%%%%
%%% REPLACE NODE %%%
%%%%%%%%%%%%%%%%%%%%
replace_node_with_anns(Node,Children) ->
	Anns = erl_syntax:get_ann(Node),
	NewNode = erl_syntax:make_tree(erl_syntax:type(Node),Children),
	erl_syntax:set_ann(NewNode,Anns).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% REPLACE NTH ELEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
replacenth(Index,Value,List) ->
 replacenth(Index-1,Value,List,[],0).

replacenth(ReplaceIndex,Value,[_|List],Acc,ReplaceIndex) ->
 lists:reverse(Acc)++[Value|List];
replacenth(ReplaceIndex,Value,[V|List],Acc,Index) ->
 replacenth(ReplaceIndex,Value,List,[V|Acc],Index+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% REPLACE AFTER THE NTH ELEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_after_position(List,Index,VarDic) ->
	replace_after_position(List,Index,[],1,VarDic).

replace_after_position([],_,New_list,_,VarDic) ->
	{lists:reverse(New_list),VarDic};
replace_after_position([H|T],Index,New_list,Pos,Dic) -> %REVISAR QUE HACER CUANDO POS = INDEX, NO CAMBIAR YA QUE SE TRATA A POSTERIORI
	case Pos > Index of
		true ->
			replace_after_position(T,Index,[gen_free_var()|New_list],Pos+1,Dic); %REPLACE WITH THE FREE VAR GENERATOR CALL
		false ->
			{NewH,NewDic} = erl_syntax_lib:mapfold(
				fun(Node,Dict) ->
					case erl_syntax:type(Node) of
						variable ->
							Ann = erl_syntax:get_ann(H),
							Bounded_vars = case Ann of
								[Info] ->
									{env,Bounded} = Info#nodeinfo.env,
									Bounded;
								[] ->
									[]
							end,
							VarName = erl_syntax:variable_name(Node),
							case lists:member(VarName,Bounded_vars) of
								true ->
									{Node,Dict};
								false ->
									Bool = dict:fold(
										fun(_,V,Acc) ->
											case Node of
												V ->
													true;
												_ ->
													false or Acc
											end
										end,
										false,
										Dict),
									case Bool of
										true ->
											{Node,Dict};
										false ->
											gen_free_var_before_sc(Node,Dict)
									end
							end;
						_ ->
							{Node,Dict}
					end
				end,
				Dic,
				H),
			replace_after_position(T,Index,[NewH|New_list],Pos+1,NewDic)
	end.

gen_free_var() ->
	Ref = make_ref(),
	var_gen ! {get_free_variable,Ref,self()},
	receive
		{FV,Ref} -> FV
	end.

gen_free_var_before_sc(Var,Dic) ->
	Var_name = erl_syntax:variable_name(Var),
	case dict:find(Var_name,Dic) of
		error ->
			New_var = gen_free_var(),
			New_dic = dict:store(Var_name,New_var,Dic),
			{New_var,New_dic};
		{ok,Value} ->
			{Value,Dic}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ADD AT NTH POSITION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_at_nth(_,_,_,[],New_list) -> lists:reverse(New_list);
add_at_nth(New_element,Add_index,Add_index,[Elem|List],New_list) ->
	add_at_nth(New_element,Add_index,Add_index+1,List,[Elem,New_element|New_list]);
add_at_nth(New_element,Add_index,Index,[Elem|List],New_list) ->
	add_at_nth(New_element,Add_index,Index+1,List,[Elem|New_list]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEBUGGING PURPOSE %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
printer(Node) -> io:format("~p\n",[Node]).
% printers(Node) -> io:format("~s\n",[erl_prettypr:format(Node)]).
% printList([]) -> 
% 	theEnd;
% printList([H|T]) ->
% 	printer(H),
% 	printer("<=============>"),
% 	printList(T).
% printLists([]) -> 
% 	printer("<=============>"),
% 	theEnd;
% printLists([H|T]) ->
% 	printers(H),
% 	printLists(T).
