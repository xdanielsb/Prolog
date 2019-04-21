% Rules
delete(_, [], []).
delete(X, [X|T], T).
delete(X, [A|T], [A|T1]) :- delete(X,T, T1).
