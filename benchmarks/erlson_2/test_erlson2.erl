-module(test_erlson2).
-compile(export_all).

poi1Old() ->
	{'erlson.erl',77, call, 1}.
poi1New() ->
	{'erlson_ok.erl', 101, call, 1}.

poi2Old() ->
	{'erlson.erl',79, 'try', 1}.
poi2New() ->
	{'erlson_ok.erl', 103, 'try', 1}.

rel1() ->
	[{poi1Old(), poi1New()}, {poi2Old(), poi2New()}].

funs() ->
	"[store/3]".
