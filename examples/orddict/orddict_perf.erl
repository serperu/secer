-module(orddict_perf).
-compile(export_all).

%% This performance test assumes the original orddict_new module is renamed to
%% orddict_old, and the revised orddict_new module is named orddict_new to replace the
%% default orddict_new. Note that the orddict_new module must be unstuck before it
%% can be replaced (code:unstick_mod(orddict_new)).

-define(ITERATIONS, [1,10,100,1000]).

start() ->
    from_list().

%% Check performance of from_list. These tests build orddicts from lists of
%% lengths 1, 10, 100, and 1000, timing 1000 iterations of each. Two
%% different input list forms are used: one a list of {int(),int()} pairs
%% in sort order, the other a list of {int(),int()} in reverse sort order.
from_list() ->
    [old_from_list(N) || N <- ?ITERATIONS],
    [new_from_list(N) || N <- ?ITERATIONS].
old_from_list(Len=1) ->
    Iter = 1000,
    {TimeFwd,_} = timer:tc(?MODULE,old_from_list,[Len,Iter,fwd]),
    io:format("old from_list length ~w: ~w~n", [Len, TimeFwd/Iter]);
old_from_list(Len) ->
    Iter = 1000,
    {TimeFwd,_} = timer:tc(?MODULE,old_from_list,[Len,Iter,fwd]),
    io:format("old from_list length ~w ordered: ~w~n", [Len, TimeFwd/Iter]),
    {TimeRev,_} = timer:tc(?MODULE,old_from_list,[Len,Iter,rev]),
    io:format("old from_list length ~w reverse ordered: ~w~n",
              [Len, TimeRev/Iter]).
old_from_list(_Len,0,_) ->
    ok;
old_from_list(1,Count,fwd) ->
    orddict_old:from_list([{42,true}]),
    old_from_list(1,Count-1,fwd);
old_from_list(Len,Count,fwd) ->
    orddict_old:from_list([{V,V} || V <- lists:seq(1,Len)]),
    old_from_list(Len,Count-1,fwd);
old_from_list(Len,Count,rev) ->
    orddict_old:from_list([{V,V} || V <- lists:seq(Len,1,-1)]),
    old_from_list(Len,Count-1,rev).
new_from_list(Len=1) ->
    Iter = 1000,
    {TimeFwd,_} = timer:tc(?MODULE,new_from_list,[Len,Iter,fwd]),
    io:format("new from_list length ~w: ~w~n", [Len, TimeFwd/Iter]);
new_from_list(Len) ->
    Iter = 1000,
    {TimeFwd,_} = timer:tc(?MODULE,new_from_list,[Len,Iter,fwd]),
    io:format("new from_list length ~w ordered: ~w~n", [Len, TimeFwd/Iter]),
    {TimeRev,_} = timer:tc(?MODULE,new_from_list,[Len,Iter,rev]),
    io:format("new from_list length ~w reverse ordered: ~w~n",
              [Len, TimeRev/Iter]).
new_from_list(_Len,0,_) ->
    ok;
new_from_list(1,Count,fwd) ->
    orddict_new:from_list([{42,true}]),
    new_from_list(1,Count-1,fwd);
new_from_list(Len,Count,fwd) ->
    orddict_new:from_list([{V,V} || V <- lists:seq(1,Len)]),
    new_from_list(Len,Count-1,fwd);
new_from_list(Len,Count,rev) ->
    orddict_new:from_list([{V,V} || V <- lists:seq(Len,1,-1)]),
    new_from_list(Len,Count-1,rev).