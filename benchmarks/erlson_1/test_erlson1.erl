-module(test_erlson1).
-compile(export_all).

poi1Old() ->
	{'erlson.erl',233, call, 1}.
poi1New() ->
	{'erlson_ok.erl', 233, call, 1}.

rel1() ->
	[{poi1Old(),poi1New()}].

funs() ->
	"[list_to_json_array/1]".
