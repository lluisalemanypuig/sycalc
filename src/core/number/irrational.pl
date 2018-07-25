:- ensure_loaded(rational).

% IRRATIONALS

/***
	@descr This file contains the definition of irrational numbers.
*/

/**
	@form irrational(A)
	@descr @A is an irrational number if it is not a rational number.
*/
irrational(A):- not(rational(A)), number(A).
