:-ensure_loaded(rational).
:-ensure_loaded(irrational).

% REALS

/***
	@descr This file contains the definition of a real number (either
	a rational or an irrational number) and the definition of absolute
	value of such a number.
*/

/**
	@form real(A)
	@descr This predicate failes if A is neither a rational or an
	irrational value.
*/
real(A):- rational(A), !.
real(A):- irrational(A).

% Compute the absolute value of a real number
/**
	@form abs_real(X, A)
	@constrs X is a real number.
	@descr A is the aboslute value of X.
*/
abs_real(X, AX):- fraction(X), X < 0, neg_frac(X, AX), !.
abs_real(X, X):- fraction(X), X > 0, !.
abs_real(X, AX):- abs(X, AX).

