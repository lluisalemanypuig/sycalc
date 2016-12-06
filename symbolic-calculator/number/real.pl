:-ensure_loaded(rational).
:-ensure_loaded(irrational).

% REALS

% Is A a real number?
real(A):- rational(A), !.
real(A):- irrational(A).

% Compute the absolute value of a real number
abs_real(X, AX):- fraction(X), X < 0, neg_frac(X, AX), !.
abs_real(X, X):- fraction(X), X > 0, !.
abs_real(X, AX):- abs(X, AX).

% UTILS

max_num(X, Y, X):- X >= Y, !.
max_num(_, Y, Y).

min_num(X, Y, X):- X =< Y, !.
min_num(_, Y, Y).