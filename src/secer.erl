-module(secer).
-export([run/1]).

run([File,Fun,OffsetStart,OffsetEnd]) ->
	try 

		register(input_gen,spawn(secer_input_gen,main,[[File,Fun,OffsetStart,OffsetEnd]])),
		register(inputs_server,spawn(secer_im_server,init,[]))

	catch 
		_:_ -> error
	end.