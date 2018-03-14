-module(test_mergesort).
-compile(export_all).

poi1Old() ->
	{'merge.erl',9, call, 1}.
poi1New() ->
	{'merge_ok.erl', 9, call, 1}.

poi1_1Old() ->
	{'merge.erl',14, call, 1}.
poi1_1New() ->
	{'merge_ok.erl', 14, call, 1}.

poi1_2Old() ->
	{'merge.erl',15, call, 1}.
poi1_2New() ->
	{'merge_ok.erl', 15, call, 1}.

poi1_3Old() ->
	{'merge.erl',16, call, 1}.
poi1_3New() ->
	{'merge_ok.erl', 16, call, 1}.

poi1_4Old() ->
	{'merge.erl',17, call, 1}.
poi1_4New() ->
	{'merge_ok.erl', 17, call, 1}.

poi1_5Old() ->
	{'merge.erl',18, call, 1}.
poi1_5New() ->
	{'merge_ok.erl', 18, call, 1}.
	
poi1_6Old() ->
	{'merge.erl',19, call, 1}.
poi1_6New() ->
	{'merge_ok.erl', 19, call, 1}.

poi2_1Old() ->
	{'merge2.erl',14, call, 1}.
poi2_1New() ->
	{'merge_ok.erl', 14, call, 1}.

poi2_2Old() ->
	{'merge2.erl',15, call, 1}.
poi2_2New() ->
	{'merge_ok.erl', 15, call, 1}.

poi2_3Old() ->
	{'merge2.erl',16, call, 1}.
poi2_3New() ->
	{'merge_ok.erl', 16, call, 1}.

poi2_4Old() ->
	{'merge2.erl',17, call, 1}.
poi2_4New() ->
	{'merge_ok.erl', 17, call, 1}.

poi2_5Old() ->
	{'merge2.erl',18, call, 1}.
poi2_5New() ->
	{'merge_ok.erl', 18, call, 1}.
	
poi2_6Old() ->
	{'merge2.erl',19, call, 1}.
poi2_6New() ->
	{'merge_ok.erl', 19, call, 1}.


rel1() ->
	[{poi1Old(),poi1New()}].

rel2() ->
	[{poi1_1Old(),poi1_1New()}, {poi1_2Old(),poi1_2New()}, {poi1_3Old(),poi1_3New()}, {poi1_4Old(),poi1_4New()}, {poi1_5Old(),poi1_5New()}, {poi1_6Old(),poi1_6New()}].

rel3() ->
	[{poi2_1Old(),poi2_1New()}, {poi2_2Old(),poi2_2New()}, {poi2_3Old(),poi2_3New()}, {poi2_4Old(),poi2_4New()}, {poi2_5Old(),poi2_5New()}, {poi2_6Old(),poi2_6New()}].

funs() ->
	"[mergesortcomp/1]".
