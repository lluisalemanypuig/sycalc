/***
	@descr This file contains the basic definitions for what a natural
	number is, two constants (zero and one), 
*/

/**
	@descr Predicate fails if parameter is not 0.
*/
zero(0).
/**
	@descr Predicate fails if parameter is not 1.
*/
one(1).

/**
	@form natural(N)
	@descr Predicates fails on any non-integer value, or negative
	integer value.
*/
natural(N):- integer(N), N >= 0.

/**
	@form next_natural(N)
	@descr N is a natural. Example of usage:
		next_natural(N), write(N), nl, fail.
*/
next_natural(0).
next_natural(N):- next_natural(M), N is M + 1.

