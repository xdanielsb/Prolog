# Prolog

Prolog is a logical and  declarative language, the name comes from PROgraming LOGic
 - Fourth generation Programing language

## Applications
 - Natural Language Understanding
 - Expert systems
 - Machine Learning
 - Specification Language


## How to

```sh
 $ swipl
 ?- [p01_relationsClass].  # this will load the program
 ...
 # execute your queries over that knowledge
```

## Theory

#### Relations
It is a property that assign a true value

  - Elon Musk is a human -> **human(elonMusk)**
  - Earth is round       -> **round(earth)**

In prolog a fact is an explicity  relation

#### Rules
It is a restriction given to a relation, help to find a fact even is not explicity and stated

 - Juan (X) is my (Y) grandfather if Juan is the father of my father(Z) or the father of my mother(Z)
  - **grandfather(X, Y):-  (father(Z, Y), father(Z, X));(father(Z, Y),mother(Z, X))**


### Syntax of prolog
 - names of relations must be stated in lower case
 - the relation name appears as the first term #signature of relation
 - objects appears in the relation separated by commas
 - Must be written a period at the end of the fact or rule
 - objects can also begin with digits
 - string also can be written enclosed in quotes
 - variables names must start with a capital letter or an underscore
 - :- means if , also called the neck symbol
 - , (comma) is called  conjuction (and) symbol
 - ; (semicolon) is called  disjunction (or) symbol



# List
Simple data structure, can be written as a Prolog atom as

L = []
