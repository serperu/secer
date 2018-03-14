-module(test_sum_digits).
-compile(export_all).

% GENERAL

file(o) -> 
	'sum_digits.erl';
file(n) -> 
	'sum_digits_ok.erl'.

poi_rel(POI) -> 
	{POI(o), POI(n)}.
poi_rel(POI1,POI2) -> 
	{POI1(), POI2()}.

% POIs

poi1(Version) ->
	{file(Version), 5, call, 1}.

poi2(Version) ->
	{file(Version), 8, call, 1}.

poi1_1(Version) ->
	{file(Version), 11, {var, 'Acc'}, 1}.

poi1_2_o() ->
	{file(o), 14, {var, 'Acc'}, 1}.

poi1_2_n() ->
	{file(n), {14,5}, {14,10}}.

poi1_3(Version) ->
	{file(Version), 16, call, 1}.


rel1() ->
	[poi_rel(fun poi1/1), poi_rel(fun poi2/1)].

rel2() ->
	[
	 poi_rel(fun poi1_1/1),
	 poi_rel(fun poi1_2_o/0,fun poi1_2_n/0),
	 poi_rel(fun poi1_3/1)
	].

rel3() ->
	[poi_rel(fun poi1_2_o/0,fun poi1_2_n/0)].

funs() ->
 	"[sum_digits/1]".

