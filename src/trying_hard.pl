map(_, [], []):- !.
map(F, [X|L], [E|R]):- call(F, X, E), map(F, L, R), !.

sum(X, Y, E):- write(X), nl, E is X + Y.

main:- L = [1,2,3,4], map(sum(4), L, R), write(R), nl.

