-module(test_rfib).
-compile(export_all).

% GENERAL

file(o) -> 
	'rfib.erl';
file(n) -> 
	'rfib_ok.erl'.

poi_rel(POI) -> 
	{POI(o), POI(n)}.


% POIs

poi1(Version) ->
	{file(Version), 46, call, 1}.

poi1_1(Version) ->
	{file(Version), 56, call, 1}.

poi1_2(Version) ->
	{file(Version), 56, call, 2}.


rel1() ->
	[poi_rel(fun poi1/1)].

rel2() ->
	[
		poi_rel(fun poi1_1/1), 
		poi_rel(fun poi1_2/1)
	].

funs() ->
	"[main/1]".

