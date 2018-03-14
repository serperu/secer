-module(test_hello_server).
-export([rel/0, funs/0]).

% GENERAL

file(0) -> 
	'hello_server.erl';
file(1) -> 
	'hello_server_wrong.erl'.

poi_rel(POI) -> 
	{POI(0), POI(1)}.

% POIs

poi1(Version) ->
	{file(Version), 75, 'tuple', 1}.

poi2(Version) ->
	{file(Version), 83, tuple, 1}.

poi3(Version) ->
	{file(Version), 90, tuple, 1}.

rel() ->
	[poi_rel(fun poi1/1), poi_rel(fun poi2/1), poi_rel(fun poi3/1)].


funs() ->
	"[handle_call/3, handle_cast/2]".

