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
element(lampara, 1, mediano, 8, yes, no).
element(platos, 1, mediano, 5, yes, no).
element(platos, 1, mediano, 5, no, no).
element(abrigo, 1, ligero, 3, no, no).
element(tv, 1, pesado, 10, yes, no).
element(pantalones, 1, ligero, 2, no, yes).
element(pantalones,2, ligero, 2, no, yes).
element(camisa, 1, ligero, 1, no, yes).
element(camisa, 2, ligero, 1, no, yes).


fragiles(L) :- findall((M,X),element(_,M,_,X,yes,_), L).                      % fragiles
pesados(L) :- findall((M,X),element(_,M,pesado,X,no,_), L).                   % pesados no fragiles
medianos(L) :- findall((M,X),element(_,M,mediano,X,no,_), L).                 % medianos no fragiles
ligeros(L) :- findall((M,X),element(_,M,ligero,X,no,_), L).                   % ligero no fragiles

decompress((0, _), []):-!.                                                    % decompress tuples into list O(max(Xi))
decompress((A,X), [(X,1)|Ans]) :-
  B is (A-1),
  decompress((B, X), Ans).

append(L,[], L):-!.                                                           % append lists O(n+m)
append([], L, L):-!.
append([X1|T1], L,[X1|T2]) :-
  append(T1, L, T2).

boxCreate([], []):-!.                                                         % Generate boxes
boxCreate( [(X,Y)|T], L) :-
  boxCreate(T, Ans1 ),
  decompress((X,Y), Ans2),
   append(Ans1, Ans2, L).

compress([], []).                                                             % Compress Boxes not restricttion of amount
compress([(X,Y)], [(X,Y)]):-!.
compress([(X,C1),(Y, C2)|T], Ans):-
  N1 is X+Y,
  CT is C1+C2,
  (
    (
      N1 =< 10,
      append([(N1,CT)], T, Ans1),
      compress(Ans1, Ans)
    );
    (
      (N1 > 10),
      (
       (X =< Y, W is X, Z is Y, C3 is C1, C4 is C2);
       (Y < X, W is Y, Z is X, C3 is C2, C4 is C1)
      ),
      append([(W, C3)], T, Ans1),
      compress(Ans1, Ans2),
      append([(Z, C4)], Ans2, Ans)
    )
  ).

compress2([], []).                                                            % Compress Boxes each box < 4 heavier elements,
compress2([(X,Y)], [(X,Y)]):-!.
compress2([(X,C1),(Y, C2)|T], Ans):-
  N1 is X+Y,
  CT is C1+C2,
  (
    (
      N1 =< 10,
      CT < 4,
      append([(N1,CT)], T, Ans1),
      compress2(Ans1, Ans)
    );
    (
      (N1 > 10; CT>=4),
      (
       (X =< Y, W is X, Z is Y, C3 is C1, C4 is C2);
       (Y < X, W is Y, Z is X, C3 is C2, C4 is C1)
      ),
      append([(W, C3)], T, Ans1),
      compress2(Ans1, Ans2),
      append([(Z, C4)], Ans2, Ans)
    )
  ).

createBoxes(HFMBoxes) :-                                                       % Create boxes {heavy,medium, fragile}
  fragiles(L1), pesados(L2), medianos(L3),                                     % create list objects, they are mutual exclusive
  boxCreate(L1, B1), boxCreate(L2, B2), boxCreate(L3, B3),                     % put them in individual boxes, divided in categories
  compress(B1, FragileBoxes), compress2(B2, C2 ),                              % Unite fragiles objects. Unite heavier objects
  append(B3, C2, HB),                                                          % Unite boxes Heavy and Medium
  compress(HB, HeavyBoxes),                                                    % Compress the last union
  append(HeavyBoxes, FragileBoxes, R1),                                        % Unite boxes
  sort(R1, HFMBoxes).                                                          % sort


joinT(B, [], Ans, Rem):-                                                       % Unite HFMBoxes with light boxes.
  compress(Rem, C),
  append(B, C, Ans).
joinT([(X,C1)|T1], [(Y,C2)|T2],Ans, Rem):-
  N is ( X + Y),
  C is ( C1+C2),
  (
    (
      N =< 10,
      sort([(N,C)|T1], A2),
      joinT(A2, T2, Ans, Rem)
    );
    (
      N > 10,
      joinT([(X,C1)|T1], T2, Ans, [(Y, C2)|Rem])
    )
  ).


init(Ans) :-                                                                   % Utility to return the boxes
  ligeros(L4), boxCreate(L4, B4),
  createBoxes(HFMBoxes),
  joinT(HFMBoxes, B4, Ans, []).


% Insert data
insert:-
 write('Name  = '),  read(Name), nl,
 write('Number = '),  read(Number), nl,
 write('Weight = (pesado/liviano) '),  read(Weight), nl,
 write('Size = '),  read(Size), nl,
 write('Is it fragil?    (yes/no) = '),  read(Fragil), nl,
 write('Is it foldeable? (yes/no) = '),  read(Folde), nl,
 assertz(element( Name, Number, Weight, Size, Fragil,Folde)).


% Show Data
list:-
  forall(element(Name, Number,_,_,_,_),(write(Number),write(' '),writeln(Name))).
listf:-
  forall(element(Name, Number,_,_,yes,_),(write(Number),write(' '),writeln(Name))).
lists:-
  forall(element(Name, Number,_,_,no,_),(write(Number),write(' '),writeln(Name))).





% Execute this init( BoxFragiles, BoxSolid).
