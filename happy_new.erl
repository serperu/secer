-module(happy_new).
-export([main/2]).
-import(lists, [map/2, member/2, sort/1, sum/1]).
is_happy(X, XS) ->
	if
		X == 1 -> true;
		X < 10 -> false;
		true ->
			case member(X, XS) of
				true -> false;
				false ->
					is_happy(sum(map(fun(Z) -> Z*Z end, 
							[Y - 48 || Y <- integer_to_list(X)])),
						 [X|XS])
			end
	end.
happy(X, Top, XS) ->
	if
		length(XS) == Top -> sort(XS);
		true ->
			case is_happy(X,[]) of
				true -> happy(X + 1, Top, [X|XS]);
				false -> happy(X + 1,Top, XS)
			end
	end.

-spec main(pos_integer(),pos_integer()) -> [pos_integer()].
main(N, M) -> 
	happy(N,M,[]).