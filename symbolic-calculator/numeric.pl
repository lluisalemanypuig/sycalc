%% NUMERICAL FUNCTIONS

% NATURALS

natural(N):- integer(N), N >= 0.

% multiple(A, B, X):- X = (A mod B == 0)
multiple(A, A):- true, !.
multiple(A, B):- D is A mod B, D == 0, !.
multiple(_, _):- false.

gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).

% INTEGRALS

% FRACTIONS

% A fraction is the quotient of two integer numbers

numerator(A/_, A).
denominator(_/B, B).
fraction_components(A, N, D):- numerator(A, N), denominator(A, D).

irreducible_fraction(A/B):- gcd(A, B, G), G is 1.

reduced_fraction(0/_, 0):- !.
reduced_fraction(A/0, A/0):- write('Warning (reduced_fraction): denominator is zero.'), nl, !.
reduced_fraction(A/1, A):- !.
reduced_fraction(A/A, 1):- !.
reduced_fraction(A/B, C):- gcd(A, B, G), G is B, !, C is A/G.
reduced_fraction(A/B, C/D):- gcd(A, B, G), C is A/G, D is B/G.

fraction_sum(A/B, C/B, E/B):- E is A + C, !.
fraction_sum(A/B, C/D, E/F):- F is B*D, E is A*D + B*C.

fraction_sub(A/B, C/B, E/B):- E is A - C, !.
fraction_sub(A/B, C/D, E/F):- F is B*D, E is A*D - B*C.

eval_fraction(A/B, C):- B \= 0, C is A/B.

% RATIONALS

fraction(A):- numerator(A, N), denominator(A, D), integer(N), natural(D).

rational(A):- integer(A), !.
rational(A):- fraction(A).

sum(A, B, C):- fraction(A), fraction(B), fraction_sum(A, B, D), reduced_fraction(D, C), !.
sum(A, B, C):- fraction(A), fraction_sum(A, B/1, D), reduced_fraction(D, C), !.
sum(A, B, C):- fraction(B), fraction_sum(A/1, B, D), reduced_fraction(D, C), !.
sum(A, B, C):- C is A + B.

% C is A - B
sub(A, B, C):- fraction(A), fraction(B), fraction_sum(A, B, D), reduced_fraction(D, C), !.
sub(A, B, C):- fraction(A), fraction_sum(A, B/1, D), reduced_fraction(D, C), !.
sub(A, B, C):- fraction(B), fraction_sum(A/1, B, D), reduced_fraction(D, C), !.
sub(A, B, C):- C is A - B.
