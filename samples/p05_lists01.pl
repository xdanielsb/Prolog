% find if an element X is in the list L

% is in head?
isIn(X,[X|_]).
% is in tail?
isIn(X, [_|Y]) :- isIn(X,Y).


demoLists(L1, L2) :-
  L1 = [0, 1, 3],
  L2 =  [0 | [1, 2]].
