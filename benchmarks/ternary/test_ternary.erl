-module(test_ternary).
-compile(export_all).

% GENERAL

file(o) -> 
	'ternary.erl';
file(n) -> 
	'ternary_ok.erl'.

poi_rel(POI) -> 
	{POI(o), POI(n)}.

% POIs

poi1(Version) ->
	{file(Version), 12, list, 1}.

poi1_1(Version) ->
	{file(Version), 7, call, 1}.

poi1_2(Version) ->
	{file(Version), 7, call, 2}.

poi2_1(Version) ->
	{file(Version), 32, {var, 'Acc'}, 1}.

poi2_2(Version) ->
	{file(Version), 34, call, 1}.

poi2_3(Version) ->
	{file(Version), 36, call, 1}.

poi2_4(Version) ->
	{file(Version), 39, call, 1}.

rel1() ->
	[poi_rel(fun poi1/1)].

rel2() ->
	[poi_rel(fun poi1_1/1), poi_rel(fun poi1_2/1)].

rel3() ->
	[poi_rel(fun poi2_1/1), poi_rel(fun poi2_2/1), poi_rel(fun poi2_3/1), poi_rel(fun poi2_4/1)].

funs() ->
	"[main/3]".

