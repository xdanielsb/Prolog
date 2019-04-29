% element( name, number, weigth, size, fragil, foldable)

author('Daniel', 'Santos').
date('April', 2019).
desc('Description', 'Greedy algorithm to pack elements of boxes').
desc('Note', 'By reduction is a subcase of knapsack problem, so NP-Hard & NP-Complete').
desc('Complexity Time', '~O(MaxV*N*N*log(N)), could be reduced to O(MaxV*N*log(N)) if I would have deciphered how to use memoization in prolog :`(, coming soon :) ').

% modules
:- use_module(library(aggregate)).

% declare dynamics
:- dynamic element/6.

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

compress([], []).                                                             % Greedy algorithm to Compress Boxes not restricttion of amount
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
      ( % greedy
       (X =< Y, W is X, Z is Y, C3 is C1, C4 is C2);
       (Y < X, W is Y, Z is X, C3 is C2, C4 is C1)
      ),
      append([(W, C3)], T, Ans1),
      compress(Ans1, Ans2),
      append([(Z, C4)], Ans2, Ans)
    )
  ).

compress2([], []).                                                            % Greedy algorithm to Compress Boxes each box < 4 heavier elements,
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
      ( % greedy
       (X =< Y, W is X, Z is Y, C3 is C1, C4 is C2);
       (Y < X, W is Y, Z is X, C3 is C2, C4 is C1)
      ),
      append([(W, C3)], T, Ans1),
      compress2(Ans1, Ans2),
      append([(Z, C4)], Ans2, Ans)
    )
  ).

createBoxes(HFMBoxes) :-                                                       %
  fragiles(L1), pesados(L2), medianos(L3),                                     % Fetch the array with  {fragiles, pesados medianos}, these arrays are mutual exclusive so their intersection among any pair is void
  boxCreate(L1, B1), boxCreate(L2, B2), boxCreate(L3, B3),                     % create a first version of not optimal boxes (all with just one element in each box ), for example if there is two glass  B1 = [(w1, 1), (w1,1)] where w1 stands out the weight, and 1 represent the quantity
  compress(B1, FragileBoxes), compress2(B2, C2 ),                              % Unite fragiles objects. Unite heavier objects compress the boxes of fragil objects
  append(B3, C2, HB),                                                          % Unite boxes Heavy and Medium, due Medium just could be with Heavy Boxes
  compress(HB, HeavyBoxes),                                                    % Compress the last union
  append(HeavyBoxes, FragileBoxes, R1),                                        % Unite boxes R1 = [HeavyBoxes, FragilBoxes]
  sort(R1, HFMBoxes).                                                          % sort increasing


joinT(B, [], Ans, Rem):-                                                       % Unite Heavy Fragil and Medium (HFM) Boxes with light boxes. Light elments could be with whatever type of element if the boxes has the enough space
  compress(Rem, C),
  append(B, C, Ans).
joinT([(X,C1)|T1], [(Y,C2)|T2],Ans, Rem):-
  N is ( X + Y),
  C is ( C1+C2),
  (
    (
      N =< 10,
      sort([(N,C)|T1], A2), % greedy
      joinT(A2, T2, Ans, Rem)
    );
    (
      N > 10,
      joinT([(X,C1)|T1], T2, Ans, [(Y, C2)|Rem])
    )
  ).


init(Desc, Size) :-                                                                   % Utility to return the boxes
  ligeros(L4), boxCreate(L4, B4),
  createBoxes(HFMBoxes),
  joinT(HFMBoxes, B4, X, []),
  sort(X, Desc),
  length(Desc, Size).


% Insert data
insert:-
 write('Please write all in lower case letters and finish the text with a period (.), after that press enter'),nl,
 write('Name  = '),  read(Name), nl,
 write('Number = '),  read(Number), nl,
 write('Weight = (pesado. or mediano . or liviano.) '),  read(Weight), nl,
 write('Size = '),  read(Size), nl,
 write('Is it fragil?    (yes or no) = '),  read(Fragil), nl,
 write('Is it foldeable? (yes or no) = '),  read(Folde), nl,
 assertz(element( Name, Number, Weight, Size, Fragil,Folde)).


% Show Data, different categories to pack boxes, given the filters in [26 29] lines
listf:-
  forall(element(Name,Number,_,_,yes,_),(write(Number),write(' '),writeln(Name))).
listp:-
  forall(element(Name,Number,pesado,_,no,_),(write(Number),write(' '),writeln(Name))).
listm:-
  forall(element(Name,Number,mediano,_,no,_),(write(Number),write(' '),writeln(Name))).
listl:-
  forall(element(Name,Number,ligero,_,no,_),(write(Number),write(' '),writeln(Name))).

:- write(' Hi User you can do the following operations in this program'),nl,nl,
   write(' \t 1. Write (insert.) without the parenthesis to insert a new register  '), nl,
   write(' \t 2. Write (init(Boxes, Size).) to see the results after  pack the elements in the the boxes,  each pair of the list  (Wi, n)  stands out a box where Wi is the weight of that box and n is the number of elements in the box'), nl,
   write(' \t 3. Write any option of (listf. or listp or listm or listl) without the parenthesis to show the list of fragiles, pesados no fragiles, medium no fragiles and light no fragiles objects respectively, (each list is mutual exclusive) '), nl.



/* test real Data
  % pack fragile objects = { 1 plato w={5}, 1 lampara w = {8}, tv w = {10} }
  BoxesF = [ (5,1), (8,1), (10,1)]

  % pack heavy objects not fragile = {1 libro w={2}, 2 libros w={2} each one, }
  BoxesH = [ (6,3)]

  % Pack medium not fragile objects in BoxesH = {}
  BoxesH = [ (6,3)]

  % Unite list BoxesF y BoxesH command createBoxes(Boxes).
  Boxes = [ (5,1),(6,3), (8,1), (10,1)]

  % Pack light objects in Boxes = {1 abrigo w ={3}, 1 pantalon w={2}, 2 pantalones w={2}, 1 camisa w={1}, 2 camisas w={1}}
  % these are packed in increasing order, command init(Boxes, Size).
  Boxes = [ (6,2),(6,3), (8,1), (10,1)] camisa w =1
  Boxes = [ (6,3),(7,3), (8,1), (10,1)] camisa w =1
  Boxes = [ (7,3),(7,4), (8,1), (10,1)] camisa w =1
  Boxes = [ (7,4),(8,1), (9,4), (10,1)] pantalon w = 2
  Boxes = [ (8,1),(9,4), (9,5) (10,1)] pantalon w = 2
  Boxes = [ (9,4),(9,5), (10,1), (10,2)] pantalon w = 2
  Boxes = [ (3,1), (9,4),(9,5), (10,1), (10,2)] abrigo w = 3
*/
