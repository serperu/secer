-module(secer_trace). 
-export([init/0, create_loop/4]).
		
init() ->
	%tracer([]).
	tracer({[],[]}).

tracer({Stack,Trace}) ->
	receive
		exit ->
			io:format("Dying..."), 
			ok;
		error ->
			io:format("---Error Execution---\n"), 
			ok;
		reset ->
			tracer({[],[]});
		{add_i, POI, Ref, V} ->
			tracer({[{Ref, V} | Stack], Trace});
		{add_c, POI, Ref, V} ->
			{CalleeArgs, NStack} = remove_same_ref(Ref, Stack),
			AI = dict:from_list([{ca, CalleeArgs}]), 
			tracer({NStack,[{POI, V, AI} | Trace]});
		{add_c, POI, Ref, V, ST} -> 
			{CalleeArgs, NStack} = remove_same_ref(Ref, Stack),
			AI = dict:from_list([{ca, CalleeArgs},{st, lists:droplast(ST)}]),
			%printer({NStack,[{POI, V, AI} | Trace]}),
			tracer({NStack,[{POI, V, AI} | Trace]});
		{add, POI, V, ST} -> 
			%printer({Stack,[{POI, V, dict:from_list([{st,lists:droplast(ST)}])} | Trace]}),
			tracer({Stack, [{POI, V, dict:from_list([{st,lists:droplast(ST)}])} | Trace]});
		{add, POI, V} ->
			tracer({Stack, [{POI, V, none} | Trace]});
		{get_results, Ref, Pid} ->
			Results = lists:reverse(Trace), 
			Pid ! {Ref, Results}, 
			tracer({[],[]});
		_ ->
			tracer({Stack,Trace})
	end;

tracer(Trace) -> % DELETE
	receive
		exit ->
			io:format("Dying..."), 
			ok;
		error ->
			io:format("---Error Execution---\n"), 
			ok;
		reset ->
			tracer([]);
		{add, POI, Value} ->
			%AI = dict:new(), 
			NewTrace = [{POI, Value, none} | Trace], 
			tracer(NewTrace);
		{get_results, Ref, Pid} ->
			Results = lists:reverse(Trace), 
			Pid ! {Ref, Results}, 
			tracer([]);
		_ ->
			tracer(Trace)
	end.


remove_same_ref(Ref,Stack) ->
	remove_same_ref(Ref, Stack,{[],[]}).

remove_same_ref(Ref, [{Ref, V} | Stack], {CalleeArgs, NStack}) ->
	remove_same_ref(Ref, Stack, {[V | CalleeArgs], Stack});
remove_same_ref(_,_,CalleeArgsStack) ->
	CalleeArgsStack.

create_loop(Parent, Module, FunName, TO) -> % Parent = node(), Module = atom(), FunName = atom(), TO = integer()
	receive									  
		{Ref, Input} ->								  % Recibir Input	
			Self = self(), 							  % Ejecutar codigo instrumentado
			Pid = spawn(
				fun() ->
					catch apply(Module, FunName, Input), 
					Self ! {finish, Ref}
				end), 
			Trace = receive
					{finish, Ref} ->
						tracer ! {get_results, Ref, self()}, 
						receive
							{Ref, Traces} -> 
								Traces
						end
			after TO*10 -> 
					exit(Pid, 0), 
					timeouted
			end, 	
			{input_gen, Parent} ! {Ref, Trace}, 	  % Enviar resultado a secer_node
			create_loop(Parent, Module, FunName, TO)  % Esperar input
	end.

printer(Node) -> io:format("~p\n", [Node]).
