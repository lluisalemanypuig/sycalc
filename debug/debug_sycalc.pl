:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(debug_integer_algs).
:-ensure_loaded(debug_lists).
:-ensure_loaded(debug_numbers).
:-ensure_loaded(debug_arithmetic_eval).
:-ensure_loaded(debug_monomials).
:-ensure_loaded(debug_polynomials).
:-ensure_loaded(debug_power_sums).

debug:-
	debug_integer_algs,
	debug_lists,
	debug_numbers,
	debug_arithmetic_evaluation,
	debug_monomials,
	debug_polynomials,
	debug_power_sums,
	true.

main:- debug, halt.
main:- nl, write('ERROR'), nl, halt.

