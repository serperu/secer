%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench7.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test dead code detection.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- This benchmark consists in a function that receives two numbers as inputs and calls
%-- another one with a conditional structure that contains a dead code statement.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b1).
-export([numbers/2]).
-spec numbers(integer(),integer()) -> integer().
numbers(A,B) -> 
	C=fn(A,B).
fn(X,Y) ->
	if	
		X>5 -> 
			Z=Y,
			X; 
		true -> 
			X+2
	end.