-module(secer).
-export([main/4,generate_inputs/2,generate_temp_file/3]).

main(File,Fun,OffsetStart,OffsetEnd) ->
	try 
		register(var_gen,spawn(secer_fv_server,init,[])),
		register(tracer,spawn(secer_trace,init,[])),

		{Inputs,Cvg} = generate_inputs(File,Fun)%,
		%generate_temp_file(File,OffsetStart,OffsetEnd),

		%execute(File,Fun,Inputs,[])

	catch 
		_:_ -> error
	after
		unregister(var_gen),
		unregister(tracer)
	end.

generate_inputs(File,Fun) ->
	code:purge(secer_input_gen),
	compile:file("secer_input_gen.erl",[]),
    code:load_abs("secer_input_gen"),
	Res = (catch secer_input_gen:main(File,Fun)),
	Res.

generate_temp_file(File,OffsetStart,OffsetEnd) ->
	code:purge(secer_criterion_manager),
    code:load_abs("secer_criterion_manager"),
	Seleceted_var = secer_criterion_manager:get_temp_file(File,OffsetStart,OffsetEnd),
	secer_criterion_manager:get_replaced_AST(File,Seleceted_var),
	Seleceted_var.

execute(_,_,[],Res) ->
	lists:reverse(Res);
execute(ProgramFile,Fun,[Input|Inputs],Res) ->
	Program = filename:basename("./tmp/"++ProgramFile,".erl"),
	ProgramSc = execute_input(Program,Fun,Input),
	execute(Program,Fun,Inputs,[{Input,ProgramSc}|Res]).

execute_input(MP,Fun,Input) ->
	catch apply(MP,Fun,Input),
	tracer ! {get_results,self()},
	PrograResult= receive
		X -> X
	end,
	PrograResult.

printer(Node) -> io:format("~p\n",[Node]).
