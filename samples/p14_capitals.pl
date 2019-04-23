% Hechos y reglas
:- nl, write( 'Write (ask.) to start'), nl.
:- dynamic capital_of/2.

capital_of(suiza, berna).
capital_of(chile, santiago).
capital_of(eeuu, washington).
capital_of(estados_unidos, washington).
capital_of(italia, roma).
capital_of(francia, paris).
capital_of(austria, viena).
capital_of(alemania, berlin).
capital_of(espagna, madrid).
capital_of(peru, lima).
capital_of(mexico, cuidad_de_mexico).

ask:-
 write('Name Country = '),  read(Pais), nl,
 write('Capital = '),  read(Capital), nl,
 assertz(capital_of(Pais,Capital)),repetir.

repetir:- write('desea incluir otro pais y su capital? (si/no) '), read(Respuesta),nl, ((Respuesta==si)->ask;fail).

undo :- capital_of(_ ,_),fail.
undo.
