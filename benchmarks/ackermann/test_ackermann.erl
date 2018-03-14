-module(test_ackermann).
-compile(export_all).

poi1Old() ->
	{'ackermann.erl',7, call, 1}.
poi1New() ->
	{'ackermann_ok.erl',5, call, 1}.

rel1() ->
	[{poi1Old(),poi1New()}].

funs() ->
	"[main/1]".
