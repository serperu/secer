%edd:dd("complex_number:calculate()").
%% Task: Complex Arithmetic
%% Author: Abhay Jain
 
-module(complex_number_ok).
-compile([export_all]).
 
-record(complex, {real, img}).
 
calculate(AR, AI, BR, BI) ->
    A = #complex{real=AR, img=AI},
    B = #complex{real=BR, img=BI},
 
    Sum = add (A, B),
    %print (Sum),
 
    Product = multiply (A, B),
    %print (Product),
 
    Negation = negation (A),
    %print (Negation),
 
    Inversion = inverse (A),
    %print (Inversion),
 
    Conjugate = conjugate (A),
    %print (Conjugate).
    
    {Sum, Product, Negation, Inversion, Conjugate}.
 
add (A, B) ->
    RealPart = A#complex.real + B#complex.real,
    ImgPart = A#complex.img + B#complex.img,
    #complex{real=RealPart, img=ImgPart}.
 
multiply (A, B) ->
    RealPart = (A#complex.real * B#complex.real) - (A#complex.img * B#complex.img),
    ImgPart = (A#complex.real * B#complex.img) + (B#complex.real * A#complex.img),
    #complex{real=RealPart, img=ImgPart}.
 
negation (A) ->
    #complex{real=-A#complex.real, img=-A#complex.img}.
 
inverse (A) ->
    C = conjugate (A),
    Mod = (A#complex.real * A#complex.real) + (A#complex.img * A#complex.img),
    RealPart = C#complex.real / Mod,
    ImgPart = C#complex.img / Mod,
    #complex{real=RealPart, img=ImgPart}.
 
conjugate (A) ->
    RealPart = A#complex.real,
    ImgPart = -A#complex.img,
    #complex{real=RealPart, img=ImgPart}.
 
print (A) ->
    if A#complex.img < 0 ->
        io:format("Ans = ~p~pi~n", [A#complex.real, A#complex.img]);
       true ->
        io:format("Ans = ~p+~pi~n", [A#complex.real, A#complex.img])
    end. 
