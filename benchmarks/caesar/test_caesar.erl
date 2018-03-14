-module(test_caesar).
-compile(export_all).

poi1Old() ->
	{'caesar.erl',35, call, 2}.
poi1New() ->
	{'caesar_ok.erl',34, call, 2}.

poi2Old() ->
	{'caesar.erl',32, {var, 'PlainText'}, 1}.
poi2New() ->
	{'caesar_ok.erl', 31, {var, 'PlainText'}, 1}.

poi3Old() ->
	{'caesar.erl',{14,3},{14,28}}.
poi3New() ->
	{'caesar_ok.erl', {13,3},{13,28}}.

rel1() ->
	[{poi1Old(),poi1New()}].

rel2() ->
	[{poi1Old(),poi1New()}, {poi2Old(), poi2New()}].

rel3() ->
	[{poi1Old(),poi1New()}, {poi2Old(), poi2New()}, {poi3Old(), poi3New()}].

rel4() ->
	[{poi3Old(),poi3New()}].

funs() ->
	"[main/2]".
