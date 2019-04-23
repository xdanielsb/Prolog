% element( name, number, weigth, size, fragil, foldable)

author('Daniel', 'Santos').
date('April', 2019).

% modules
:- use_module(library(aggregate)).

% declare dynamics
:- dynamic element/6.

% facts
element(libro, 1, pesado, 2, no, no).
element(libro, 2, pesado, 2, no, no).
element(lampara, 2, mediano, 8, yes, no).
element(platos, 1, mediano, 5, yes, no).
element(abrigo, 1, ligero, 3, no, no).
element(tv, 1, pesado, 10, yes, no).
element(pantalones, 1, ligero, 2, no, yes).
element(pantalones,2, ligero, 2, no, yes).
element(camisa, 1, ligero, 1, no, yes).
element(camisa, 2, ligero, 1, no, yes).

% generate list of types O(n)
fragiles(L) :- findall((M,X),element(_,M,_,X,yes,_), L).
pesados(L) :- findall((M,X),element(_,M,_,X,no,_), L).

% decompress tuples into list O(max(Xi))
decompress((0, _), []):-!.
decompress((A,X), [X|Ans]) :- B is (A-1), decompress((B, X), Ans).

% append lists O(n+m)
append(L,[], L):-!.
append([], L, L):-!.
append([X1|T1], L,[X1|T2]) :- append(T1, L, T2).

% Generate boxes
boxCreate([], []):-!.
boxCreate( [(X,Y)|T], L) :- boxCreate(T, Ans1 ), decompress((X,Y), Ans2), append(Ans1, Ans2, L).

% Compress Boxes
compress([], []).
compress([X], [X]):-!.
compress([X,Y|T], Ans):-
  N1 is X+Y,
  (
    (
      N1 =< 10,
      append([N1], T, Ans1),
      compress(Ans1, Ans)
    );
    (
      N1 > 10,((X =< Y, W is X, Z is Y);(Y < X, W is Y, Z is X) ),
      append([W], T, Ans1),
      compress(Ans1, Ans2),
      append([Z], Ans2, Ans)
    )
  ).

%  Unite the algorithm, omg the tricky part
init(R1, R2) :-
  fragiles(L1), pesados(L2), boxCreate(L1, B1), boxCreate(L2, B2), compress(B1, R1), compress(B2, R2).


% Insert data
insert:-
 write('Name  = '),  read(Name), nl,
 write('Number = '),  read(Number), nl,
 write('Weight = (pesado/liviano) '),  read(Weight), nl,
 write('Size = '),  read(Size), nl,
 write('Is it fragil?    (yes/no) = '),  read(Fragil), nl,
 write('IS it foldeable? (yes/no) = '),  read(Folde), nl,
 assertz(element( Name, Number, Weight, Size, Fragil,Folde)).


% Show Data
list:-
  forall(element(Name, Number,_,_,_,_),(write(Number),write(' '),writeln(Name))).
listf:-
  forall(element(Name, Number,_,_,yes,_),(write(Number),write(' '),writeln(Name))).
lists:-
  forall(element(Name, Number,_,_,no,_),(write(Number),write(' '),writeln(Name))).





% Execute this init( BoxFragiles, BoxSolid).
