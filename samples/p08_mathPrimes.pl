% find primes

isPrime(X, Aux) :- X = Aux. 
isPrime( X, Aux ) :- X >= 2, not( 0 is mod(X, Aux)); Aux is Aux+1, isPrime(X, Aux+1).
