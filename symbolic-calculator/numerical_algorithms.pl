
% NUMERICAL ALGORITHMS

% A is multiple of B
multiple(A, A):- true, !.
multiple(A, B):- D is A mod B, D == 0, !.
multiple(_, _):- false.

gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).

