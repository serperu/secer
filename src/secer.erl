-module(secer).
-export([run/1]).

run([File,Fun,OffsetStart,OffsetEnd]) ->
	try 

		register(input_gen,spawn(secer_input_gen,main,[[File,Fun,OffsetStart,OffsetEnd]])),
		register(input_manager,spawn(secer_im_server,init,[])),

		timer:sleep(15000),

		input_manager ! {get_results,self()},
		receive
			{Empty,Valued,Cvg} -> 
				printer("Empty inputs"),
				printer(dict:size(Empty)),
				printer("Valued inputs"),
				printer(dict:size(Valued)),
				printer("Coverage"),
				printer(Cvg);
			_ ->
				printer(error),
				error
		end
	catch 
		_:_ -> error
	after
		case {whereis(input_gen),whereis(input_manager)} of
			{undefined,undefined} ->
				ok;
			{undefined,Pid} ->
				unregister(input_manager),
				timer:exit_after(0,Pid,kill);
			{Pid,undefined} ->
				unregister(input_gen),
				timer:exit_after(0,Pid,kill);
			{Pid1,Pid2} ->
				unregister(input_manager),
				unregister(input_gen),
				timer:exit_after(0,Pid1,kill),
				timer:exit_after(0,Pid2,kill)
		end
	end.

printer(X) -> io:format("~p\n",[X]).