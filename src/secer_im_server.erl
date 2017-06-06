-module(secer_im_server).
-export([init/0]).

init() ->
	loop().

loop() ->
	receive
		_ ->
			loop()
	end.