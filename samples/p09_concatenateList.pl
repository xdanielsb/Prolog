% code to concatenate two list
show([]):- write('\n').
show([X|Tail]):- write(X), show(Tail).
concatenate([],L, L).
concatenate([X1|T1], L2, [X1|T3]) :- concatenate(T1, L2, T3).
