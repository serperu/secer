-module(secer_cc_server).
-export([init/0]).

init() ->
	loop(dict:from_list([{id_poi_dict,dict:new()}])).

loop(D) ->
	receive 
		{put, {K, V}} ->
			loop(dict:store(K,V,D));
		{get, K, Ref, Pid} ->
			V = dict:fetch(K,D),
			Pid ! {Ref,V},
			loop(D);
		die ->
			die
	end.

printer(X) ->
	io:format("~p\n",[X]).
