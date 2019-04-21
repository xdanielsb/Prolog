% fact
inword(0, 'zéro').
inword(1, 'un').
inword(2, 'deux').
inword(3, 'trois').
inword(4, 'quatre').
inword(5, 'cinq').
inword(6, 'six').
inword(7, 'sept').
inword(8, 'huit').
inword(9, 'neuf').

inwords([],[]).
inwords([X1|T1], [X2| T2]) :- inword(X1, X2), inwords(T1, T2).
