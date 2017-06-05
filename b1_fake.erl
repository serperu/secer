-module(b1_fake).
-export([numbers/1,numbers2/0]).

%numbers([A,B]) -> 
numbers([Mod,Fun,Input1,Input2]) -> 
	cuter:run(list_to_atom(Mod),list_to_atom(Fun),
		[list_to_integer(Input1),list_to_integer(Input2)],25,[{number_of_pollers,1},{number_of_solvers,1}]).
	%C=fn(atom_to_integerA,B).

numbers2() -> 
	register(cuterIn,spawn(secer_trace,init,[])),
	cuter:run(b1,numbers,[1,2],25,[{number_of_pollers,1},{number_of_solvers,1}]).
fn(X,Y) ->
	if	
		X>5 -> 
			Z=Y,
			X; 
		true -> 
			X+2
	end.