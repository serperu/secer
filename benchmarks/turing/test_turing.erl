-module(test_turing).
-compile(export_all).

% GENERAL

file(o) -> 
	'turing.erl';
file(n) -> 
	'turing_ok.erl'.

poi_rel(POI) -> 
	{POI(o), POI(n)}.

% POIs

poi1(Version) ->
	{file(Version), 39, tuple, 1}.

poi1_1(Version) ->
	{file(Version), 56, call, 1}.

poi1_2(Version) ->
	{file(Version), 57, call, 1}.

poi2_1(Version) ->
	{file(Version), 66, call, 1}.

poi2_2(Version) ->
	{file(Version), 67, call, 1}.

poi2_3(Version) ->
	{file(Version), 68, call, 1}.

poi2_4(Version) ->
	{file(Version), 69, call, 1}.

poi3_1(Version) ->
	{file(Version), 77, tuple, 1}.

poi3_2(Version) ->
	{file(Version), 78, tuple, 2}.

poi3_3(Version) ->
	{file(Version), 80, {var, 'Tape'}, 1}.

poi3_4(Version) ->
	{file(Version), 81, tuple, 2}.

poi3_5(Version) ->
	{file(Version), 82, tuple, 2}.


rel1() ->
	[poi_rel(fun poi1/1)].

rel2() ->
	[poi_rel(fun poi1_1/1), poi_rel(fun poi1_2/1)].

rel3() ->
	[poi_rel(fun poi2_1/1), poi_rel(fun poi2_2/1),
	 poi_rel(fun poi2_3/1), poi_rel(fun poi2_4/1)].

rel4() ->
	[poi_rel(fun poi3_1/1), poi_rel(fun poi3_2/1),
	 poi_rel(fun poi3_3/1), poi_rel(fun poi3_4/1), poi_rel(fun poi3_5/1)].

funs() ->
	"[main/2]".

