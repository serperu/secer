-module(test_string).
-compile(export_all).

poi1Old() ->
	{'string_old.erl',224,call,1}.
poi1New() ->
	{'string_new.erl',224,'case',1}.

poi2Old() ->
	{'string_old.erl',228,call,1}.
poi2New() ->
	{'string_new.erl',237,call,1}.
poi3New() ->
	{'string_new.erl',253,call,1}.

poi_performance_old() ->
	{'string_old.erl',226,call,1}.
poi_performance_new() ->
	{'string_new.erl',236,call,1}.

rel1() ->
	[{poi1Old(),poi1New()}].
rel12() ->
	[{poi2Old(),poi2New()},{poi2Old(),poi3New()}].


performance_rel() ->
	[{poi_performance_old(),poi_performance_new()}].

funs() ->
	"[tokens/2]".

comp_perf({_OldPoi,OldTrace}, {_NewPoi,NewTrace}) -> 
	case NewTrace =< OldTrace of
		true ->
			true;
		_ ->
			{false,"The new version couldn't improve the performance"}
	end.
