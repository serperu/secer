-module(test_roman).
-compile(export_all).

% GENERAL

file(o) -> 
	'roman.erl';
file(n) -> 
	'roman_ok.erl'.

poi_rel(POI) -> 
	{POI(o), POI(n)}.


% POIs

poi1(Version) ->
	{file(Version), 26, call, 1}.

poi1_1(Version) ->
	{file(Version), 5, call, 1}.

poi1_2(Version) ->
	{file(Version), 7, call, 1}.

poi1_3(Version) ->
	{file(Version), 8, call, 1}.

poi1_4(Version) ->
	{file(Version), 10, call, 1}.

poi1_5(Version) ->
	{file(Version), 11, call, 1}.

poi1_6(Version) ->
	{file(Version), 12, call, 1}.

poi2_1(Version) ->
	{file(Version), 14, list, 1}.
poi2_2(Version) ->
	{file(Version), 15, list, 1}.
poi2_3(Version) ->
	{file(Version), 16, list, 1}.
poi2_4(Version) ->
	{file(Version), 17, list, 1}.
poi2_5(Version) ->
	{file(Version), 18, list, 1}.
poi2_6(Version) ->
	{file(Version), 19, list, 1}.
poi2_7(Version) ->
	{file(Version), 20, list, 1}.
poi2_8(Version) ->
	{file(Version), 22, list, 1}.
poi2_9(Version) ->
	{file(Version), 23, list, 1}.

rel1() ->
	[poi_rel(fun poi1/1)].

rel2() ->
	[poi_rel(fun poi1_1/1), poi_rel(fun poi1_2/1), poi_rel(fun poi1_3/1), poi_rel(fun poi1_4/1), poi_rel(fun poi1_5/1), poi_rel(fun poi1_6/1)].

rel3() ->
	[poi_rel(fun poi2_1/1), poi_rel(fun poi2_2/1), poi_rel(fun poi2_3/1), poi_rel(fun poi2_4/1), poi_rel(fun poi2_5/1), 
	 poi_rel(fun poi2_6/1), poi_rel(fun poi2_7/1), poi_rel(fun poi2_8/1), poi_rel(fun poi2_9/1)].

funs() ->
	"[main/1]".

