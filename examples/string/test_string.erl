-module(test_string).
-compile(export_all).

poiOld() ->
	{'string_old.erl',224,call,1}.
poiNew() ->
	{'string_new.erl',224,'case',1}.

poiOldError() ->
	{'string_old.erl',228,call,1}.
poiNewError1() ->
	{'string_error.erl',237,call,1}.
poiNewError2() ->
	{'string_error.erl',252,call,1}.

poiOldPerformance() ->
	{'string_old_perf.erl',226,call,1}.
poiNewPerformance() ->
	{'string_new_perf.erl',236,call,1}.

rel() ->
	[{poiOld(),poiNew()}].
relError() ->
	[{poiOldError(),poiNewError1()},{poiOldError(),poiNewError2()}].
relPerformance() ->
	[{poiOldPerformance(),poiNewPerformance()}].

funs() ->
	"[tokens/2]".