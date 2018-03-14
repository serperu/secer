-module(orddict_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

from_list_old_test() ->
    from_list_test_common(orddict_old).

from_list_new_ok_test() ->
    from_list_test_common(orddict_new_ok).

from_list_new_wrong_test() ->
    from_list_test_common(orddict_new_wrong).

from_list_test_common(Mod) ->
    ?assertEqual(
        [{0,1}, {1, 2}, {2, 3}], 
        Mod:from_list([{0,1}, {1, 2}, {2, 3}])),
    ?assertEqual(
        [{0,2}, {2, 3}], 
        Mod:from_list([{0,1}, {0, 2}, {2, 3}])),
    ?assertError(
        function_clause, 
        Mod:from_list([1, {1, 2}, {2, 3}])).

from_list_old_test_vs_new_wrong_test() ->
    from_list_vs(orddict_old, orddict_new_wrong).

from_list_old_test_vs_new_ok_test() ->
    from_list_vs(orddict_old, orddict_new_ok).    

from_list_vs(Mod1, Mod2) ->
    Case1 = 
        [{0,1}, {1, 2}, {2, 3}],
    Case2 = 
        [{0,1}, {0, 2}, {2, 3}],
    Case3 = 
        [1, {1, 2}, {2, 3}],
    ?assertEqual(
        Mod1:from_list(Case1), 
        Mod2:from_list(Case1)),
    ?assertEqual(
        Mod1:from_list(Case2), 
        Mod2:from_list(Case2)),
    ?assertEqual(
        Mod1:from_list(Case3), 
        Mod2:from_list(Case3)).


