-module(orddict_secer_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

secer_failing_test() ->
    ?assertEqual(
        orddict_old:from_list([{{},-9},{{},3.1981469696010247}]),
        orddict_new_wrong:from_list([{{},-9},{{},3.1981469696010247}])).


