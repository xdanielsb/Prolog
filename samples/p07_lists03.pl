% sum elements in the list
sum([],0).
sum([X|Tail], Sum) :- sum(Tail, Sum2), Sum is (Sum2 + X).
