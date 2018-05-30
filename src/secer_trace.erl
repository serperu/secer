-module(secer_trace). 
-export([init/0, create_loop/4]).
		
init() ->
	loop([]).

loop(Trace) ->
	receive
		exit ->
			io:format("Dying..."), 
			ok;
		error ->
			io:format("---Error Execution---\n"), 
			ok;
		reset ->
			loop([]);
		{add, {POI, Value}} ->
			%AI = dict:new(), 
			NewTrace = [{POI, Value, none} | Trace], 
			loop(NewTrace);
		{get_results, Ref, Pid} ->
			Results = lists:reverse(Trace), 
			Pid ! {Ref, Results}, 
			loop([]);
		_ ->
			loop(Trace)
	end.

create_loop(Parent, Module, FunName, TO) -> % Parent = node(), Module = atom(), FunName = atom(), TO = integer()
	receive		
		%Start = os:timestamp(), 							  
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
			%printer({"", }), 
			{input_gen, Parent} ! {Ref, Trace}, 					  % Enviar resultado a secer_node
			create_loop(Parent, Module, FunName, TO)							 		  % Esperar input
	end.

printer(Node) -> io:format("~p\n", [Node]).
