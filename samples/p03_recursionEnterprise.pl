% facts

boss(juan, maria).
boss(carlos, miguel).
boss(maria, enrique).
boss(enrique, jhon).
boss(maria, patrick).
boss(patrick, nicky).

% rules
superior(X,Y) :- boss(X,Y).
superior(X,Y) :- boss(X,Z), superior(Z,Y).
