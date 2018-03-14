-module(orddict_t2).
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

cmd() ->
    frequency([{2,is_key},{1,from_list},{3,fetch},{5,find},{7,store}]).

key_maybe(InputList) ->
    {Keys,_} = lists:unzip(InputList),
    frequency([{9,elements(Keys)},{1,'BADKEY'}]).

input_gen()->
    ?LET({InputList,Cmd},{input_list(),list(cmd())},
         {InputList,Cmd}).

prop_equivalent_dict_modules() ->
    ?FORALL({InputList,Cmds},input_gen(),
            begin
                Old0 = orddict_old:from_list(InputList),
                New0 = orddict_new_wrong:from_list(InputList),
                {Res, _, _} = try
                                  lists:foldl(fun(_, {false, _, _}=X) -> throw(X);
                                                 (Cmd, {_, Old, New}) ->
                                                      true = (orddict_old:to_list(Old) =:= orddict_new_wrong:to_list(New)),
                                                      do(Cmd,Old,New)
                                              end, {true, Old0, New0}, Cmds)
                              catch
                                  throw:X ->
                                      X
                              end,
                Res
            end).

do(is_key,Old,New) ->
    Key = key_maybe(orddict_old:to_list(Old)),
    {orddict_old:is_key(Key, Old) =:= orddict_new_wrong:is_key(Key, New), Old, New};
do(from_list,Old,New) ->
    {orddict_old:from_list(Old) =:= orddict_new_wrong:from_list(New), Old, New};
do(fetch,Old,New) ->
    Key = key_maybe(orddict_old:to_list(Old)),
    Rold = try orddict_old:fetch(Key,Old) catch C1:E1 -> {C1,E1} end,
    Rnew = try orddict_new_wrong:fetch(Key,New) catch C2:E2 -> {C2,E2} end,
    {Rold =:= Rnew, Old, New};
do(find,Old,New) ->
    Key = key_maybe(orddict_old:to_list(Old)),
    {orddict_old:find(Key,Old) =:= orddict_new_wrong:find(Key,New), Old, New};
do(store,Old0,New0) ->
    Key = key_maybe(orddict_old:to_list(Old0)),
    Value = value(),
    Old = orddict_old:store(Key,Value,Old0),
    New = orddict_new_wrong:store(Key,Value,New0),
    {orddict_old:to_list(Old) =:= orddict_new_wrong:to_list(New), Old, New}.

test() ->
    test(?NUM_TESTS).

test(NumTests) ->
    proper:quickcheck(proper:numtests(NumTests, prop_equivalent_dict_modules())).