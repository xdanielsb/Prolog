% element( name, number, weigth, size, fragil, foldable)
% test element(_,_,_,_,_,_)

:- use_module(library(aggregate)).

% facts
element(libro, 1, pesado, 2, no, no).
element(libro, 2, pesado, 2, no, no).
element(lampara, 1, mediano, 8, yes, no).
element(platos, 1, mediano, 5, yes, no).
element(abrigo, 1, ligero, 3, no, no).
element(tv, 1, pesado, 10, yes, no).
element(pantalones, 1, ligero, 2, no, yes).
element(pantalones,2, ligero, 2, no, yes).
element(camisa, 1, ligero, 1, no, yes).
element(camisa, 2, ligero, 1, no, yes).

% generate list of types
fragiles(L) :- findall((M,X),element(_,M,_,X,yes,_), L).
pesados(L) :- findall((M,X),element(_,M,_,X,no,_), L).

% decompress tuples into list
decompress((0, _), []):-!.
decompress((A,X), [X|Ans]) :- B is (A-1), decompress((B, X), Ans).

% append lists
append([], L, L):-!.
append([X1|T1], L,[X1|T2]) :- append(T1, L, T2).

% Generate boxes
boxFragils([], []):-!.
boxFragils( [(X,Y)|T], L) :- boxFragils(T, Ans1 ), decompress((X,Y), Ans2), append(Ans1, Ans2, L).

% Compress Boxes
%compress([], L, L).
compress([X], [X],_):-!.
compress([X,Y|T], [N1|L]):-
  N1 is X+Y,
  N1 <= 10,
  compress(T, L).
