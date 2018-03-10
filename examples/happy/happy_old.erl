-module(happy_old).
-include_lib("eunit/include/eunit.hrl").
-export([main/2]).
% -compile(export_all).

-spec main(pos_integer(),pos_integer()) -> [pos_integer()].
main(N, M) -> 
	happy_list(N, M, []).

happy_list(_, N, L) when length(L) =:= N -> lists:reverse(L);
happy_list(X, N, L) ->
	Happy = is_happy(X),
	if Happy -> happy_list(X + 1, N, [X|L]);
	true -> happy_list(X + 1, N, L) end.

is_happy(1) -> true;
is_happy(4) -> false;
is_happy(N) when N > 0 ->
	N_As_Digits = [Y - 48 || Y <- integer_to_list(N)],
	is_happy(lists:foldl(fun(X, Sum) -> (X * X) + Sum end, 0, N_As_Digits));
is_happy(_) -> false.

%%%%%%%%%%%%%%%%%%%%
% TESTS
%%%%%%%%%%%%%%%%%%%%

is_happy_1_test() ->
	?assertEqual(is_happy(1), true).

is_happy_2_test() ->
	?assertEqual(is_happy(3), false).

is_happy_3_test() ->
	?assertEqual(is_happy(10), true).

is_happy_4_test() ->
	?assertEqual(is_happy(965), false).

happy_list_1_test() ->
	?assertEqual(happy_list(10, 3, [1,2,5]), [5,2,1]).

main_1_test() ->
	?assertEqual(main(0,0), []).

main_2_test() ->
	?assertEqual(main(100, 5), [100,103,109,129,130]).

