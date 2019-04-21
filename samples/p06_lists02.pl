% Count the elements in a list
len([], 0).
len([_|Tail], N) :- len(Tail, N1), N is N1+1.
