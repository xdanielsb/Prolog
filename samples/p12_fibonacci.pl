fibo(1, 1):-!.
fibo(2, 2):-!.
fibo(X, Ans) :-  N1 is X-1, N2 is X-2, fibo(N1, Ans2), fibo(N2, Ans3), Ans is (Ans2+Ans3).
