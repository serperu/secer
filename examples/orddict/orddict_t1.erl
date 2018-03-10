-module(orddict_t1).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-define(NUM_TESTS, 1000).

term_concrete() ->
    oneof([bool(),char(),int(),largeint(),real(),binary(),list(char())]).

key() ->
    term_concrete().

value() ->
    term_concrete().

input_list() ->
    non_empty(list({key(), value()})).

prop_equivalent_dict_modules() ->
    ?FORALL(InputList,input_list(),
            begin
                Old = orddict_old:from_list(InputList),
                New = orddict_new_wrong:from_list(InputList),
                true = (orddict_old:to_list(Old) =:= orddict_new_wrong:to_list(New)),
                from_list(Old,New)
            end).

from_list(Old,New) ->
    orddict_old:from_list(Old) =:= orddict_new_wrong:from_list(New).

test() ->
    test(?NUM_TESTS).

test(NumTests) ->
    proper:quickcheck(proper:numtests(NumTests, prop_equivalent_dict_modules())).