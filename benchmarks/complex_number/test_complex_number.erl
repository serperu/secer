-module(test_complex_number).
-compile(export_all).

poi1Old() ->
	{'complex_number.erl',29, tuple, 1}.
poi1New() ->
	{'complex_number_ok.erl', 29, tuple, 1}.

poi2Old() ->
	{'complex_number.erl',48, {var,'RealPart'}, 1}.
poi2New() ->
	{'complex_number_ok.erl', 47, {var,'RealPart'}, 1}.

poi3Old() ->
	{'complex_number.erl',17, call, 1}.
poi3New() ->
	{'complex_number_ok.erl', 17, call, 1}.

poi4Old() ->
	{'complex_number.erl',20, call, 1}.
poi4New() ->
	{'complex_number_ok.erl', 20, call, 1}.

poi5Old() ->
	{'complex_number.erl',23, call, 1}.
poi5New() ->
	{'complex_number_ok.erl', 23, call, 1}.

poi6Old() ->
	{'complex_number.erl',26, call, 1}.
poi6New() ->
	{'complex_number_ok.erl', 26, call, 1}.
 
rel1() ->
	[{poi1Old(),poi1New()}].
rel2() ->
	[{poi2Old(),poi2New()}, {poi3Old(),poi3New()}, {poi4Old(),poi4New()}, {poi5Old(),poi5New()}, {poi6Old(),poi6New()}].
rel3() ->
	[{poi2Old(),poi2New()}].

funs() ->
	"[calculate/4]".
