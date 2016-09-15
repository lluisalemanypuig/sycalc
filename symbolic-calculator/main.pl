:-ensure_loaded(arithmetic_evaluation).
:-ensure_loaded(polynomial_evaluation).
:-ensure_loaded(numerical_algorithms).
:-ensure_loaded(monomial_evaluation).
:-ensure_loaded(polynomials).
:-ensure_loaded(monomials).
:-ensure_loaded(numbers).
:-ensure_loaded(debug).
:-ensure_loaded(lists).

debug:-
	debug_numeric,
	debug_monomials,
	debug_polynomials.

main:- debug, halt.
main:- nl, write('ERROR'), nl, halt.