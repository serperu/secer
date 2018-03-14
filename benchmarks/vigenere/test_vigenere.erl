-module(test_vigenere).
-compile(export_all).

% GENERAL

file(o) -> 
	'vigenere.erl';
file(n) -> 
	'vigenere_ok.erl'.

poi_rel(POI) -> 
	{POI(o), POI(n)}.

% POIs

poi1(Version) ->
	{file(Version), 58, call, 1}.

poi2_1(Version) ->
	{file(Version), 54, call, 1}.

poi2_2(Version) ->
	{file(Version), 55, call, 1}.

poi3_1(Version) ->
	{file(Version), 55, call, 2}.

poi3_2(Version) ->
	{file(Version), 55, call, 4}.

poi4_1(Version) ->
	case Version of 
		o ->
			{file(Version), 40, {var,'List'}, 3};
		n ->
			{file(Version), 42, {var,'Take'}, 1}
	end.

poi4_2(Version) ->
	{file(Version), 43, call, 1}.

rel1() ->
	[poi_rel(fun poi1/1)].

rel2() ->
	[poi_rel(fun poi2_1/1), poi_rel(fun poi2_2/1)].

rel3() ->
	[poi_rel(fun poi3_1/1), poi_rel(fun poi3_2/1)].

rel4() ->
	[poi_rel(fun poi4_1/1), poi_rel(fun poi4_2/1)].

funs() ->
	"[encrypt/2]".

