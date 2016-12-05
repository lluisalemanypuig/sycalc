:-ensure_loaded(arithmetic_evaluation).
:-ensure_loaded(polynomial_evaluation).
:-ensure_loaded(monomial_evaluation).
:-ensure_loaded(integer_algorithms).
:-ensure_loaded(polynomials).
:-ensure_loaded(monomials).
:-ensure_loaded(numbers).
:-ensure_loaded(lists).

debug:-
	debug_integer_algs,
	debug_lists,
	debug_numeric,
	debug_monomials,
	debug_polynomials.

main:- debug, halt.
main:- nl, write('ERROR'), nl, halt.

output_text(OBT, EXP):- write(OBT), write(' | Not correct - Expected to see '), write(EXP), nl, false.

/*
-----------------------------
DEBUG - INTEGER ALGORITHMS
*/

deb_divisors(_, N, RES):- divisors(N, R), R == RES, !.
deb_divisors(I, N, RES):- divisors(N, R), write(I), write(' Divisors( '), write(N), write(' ): '), output_text(R, RES).

deb_gcd(_, A, B, RES):- gcd(A, B, R), R == RES, !.
deb_gcd(I, A, B, RES):- gcd(A, B, R), write(I), write(' gcd('), write(A), write(','), write(B), write(')= '), output_text(R, RES).

debug_integer_algs:-
	write('-- INTEGER ALGORITHMS DEBUG --'), nl,
	write('* DIVISORS'),

	deb_divisors(' 1)', 2, [1,-1,2,-2]),
	deb_divisors(' 2)', 3, [1,-1,3,-3]),
	deb_divisors(' 3)', 6, [1,-1,2,-2,3,-3,6,-6]),
	deb_divisors(' 4)', 8, [1,-1,2,-2,4,-4,8,-8]),
	deb_divisors(' 5)', 12, [1,-1,2,-2,3,-3,4,-4,6,-6,12,-12]),
	deb_divisors(' 6)', 20, [1,-1,2,-2,4,-4,5,-5,10,-10,20,-20]),
	deb_divisors(' 7)', 24, [1,-1,2,-2,3,-3,4,-4,6,-6,8,-8,12,-12,24,-24]),
	deb_divisors(' 8)', 32, [1,-1,2,-2,4,-4,8,-8,16,-16,32,-32]),
	deb_divisors(' 9)', 576, [1,-1,2,-2,3,-3,4,-4,6,-6,8,-8,9,-9,12,-12,16,-16,18,-18,24,-24,32,-32,36,-36,48,-48,64,-64,72,-72,96,-96,144,-144,192,-192,288,-288,576,-576]),

	write(' - OK'), nl,
	write('* GREATEST COMMON DIVISOR'),

	deb_gcd(' 1)', 1, 1, 1),
	deb_gcd(' 2)', 2, 1, 1),
	deb_gcd(' 3)', 1, 2, 1),
	deb_gcd(' 4)', 3, 3, 3),
	deb_gcd(' 5)', 7, 3, 1),
	deb_gcd(' 6)', 3, 7, 1),
	deb_gcd(' 7)', 8, 4, 4),
	deb_gcd(' 8)', 4, 8, 4),
	deb_gcd(' 9)', 12, 8, 4),
	deb_gcd(' 10)', 8, 12, 4),
	deb_gcd(' 11)', 8, 16, 8),
	deb_gcd(' 12)', 120, 49, 1),
	deb_gcd(' 13)', 5040, 49, 7),
	deb_gcd(' 14)', 42, 49, 7),
	deb_gcd(' 15)', 247, 270, 1),

	write(' - OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - LIST OPERATIONS
*/

deb_insertion_sort(_, L):- isort(L, R), msort(L, RES), R == RES, !.
deb_insertion_sort(I, L):- isort(L, R), msort(L, RES), write(I), write(L), write(' -> '), output_text(R, RES).

debug_lists:-
	write('-- LISTS OPERATIONS DEBUG --'), nl,
	write('* SORTING ALGORITHMS - INSERTION SORT'),

	deb_insertion_sort(' 1) ', [3,2,1]),
	deb_insertion_sort(' 2) ', [123,4,46,7,578,67,8567,58,21,23,4,245,3,2,1]),
	deb_insertion_sort(' 3) ', [-4,8567,25,123,4,46,7,-8,578,67,-4,8567,58,21,-7,23,4,245,3,2,1,8567]),

	write(' - OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - ARITHMETIC EVALUATION
*/

deb_red_frac(_, F, RES):- red_frac(F, R), R == RES, !.
deb_red_frac(I, F, RES):- red_frac(F, R), write(I), write(' '), write(F), write(' = '), output_text(R, RES).

deb_sum(_, A, B, RES):- eval_sum(A, B, R), RES == R, !.
deb_sum(I, A, B, RES):- eval_sum(A, B, R), write(I), write(' '), write(A), write(' + '), write(B), write(' = '), output_text(R, RES).

deb_sub(_, A, B, RES):- eval_sub(A, B, R), R == RES, !.
deb_sub(I, A, B, RES):- eval_sub(A, B, R), write(I), write(' '), write(A), write(' - '), write(B), write(' = '), output_text(R, RES).

deb_prod(_, A, B, RES):- eval_prod(A, B, R), R == RES, !.
deb_prod(I, A, B, RES):- eval_sub(A, B, R), write(I), write(' '), write(A), write(' * '), write(B), write(' = '), output_text(R, RES).

deb_power(_, A, B, RES):- eval_pow(A, B, R), R == RES, !.
deb_power(I, A, B, RES):- eval_pow(A, B, R), write(I), write(' '), write(A), write('^'), write(B), write(' = '), output_text(R, RES).

deb_arithm(_, E, RES):- arithmetic_eval(E, R), R == RES, !.
deb_arithm(I, E, RES):- arithmetic_eval(E, R), write(I), write(' '), write(E), write(' = '), output_text(R, RES).

deb_abs(_, E, RES):- abs_real(E, R), R == RES, !.
deb_abs(I, E, RES):- abs_real(E, R), write(I), write(' |'), write(E), write('| = '), output_text(R, RES), !.

debug_numeric:-
	write('-- ARITHMETIC EVALUATION DEBUG --'), nl,
	write('* REDUCED FRACTIONS'),

	deb_red_frac(' 1)', 0/1, 0),
	deb_red_frac(' 2)', 0/5, 0),
	deb_red_frac(' 3)', 1/2, 1/2),
	deb_red_frac(' 4)', 4/2, 2),
	deb_red_frac(' 5)', 5/2, 5/2),
	deb_red_frac(' 6)', 6/2, 3),
	deb_red_frac(' 7)', 6/4, 3/2),
	deb_red_frac(' 8)', 6/8, 3/4),
	deb_red_frac(' 9)', 6/10, 3/5),
	deb_red_frac(' 10)', -0/1, 0),
	deb_red_frac(' 11)', -0/5, 0),
	deb_red_frac(' 12)', -1/2, -1/2),
	deb_red_frac(' 13)', -4/2, -2),
	deb_red_frac(' 14)', -5/2, -5/2),
	deb_red_frac(' 15)', -6/2, -3),
	deb_red_frac(' 16)', -6/4, -3/2),
	deb_red_frac(' 17)', -6/8, -3/4),
	deb_red_frac(' 18)', -6/10, -3/5),
	
	write(' - OK'), nl,
	write('* SUMS'),

	deb_sum(' 1)', 0, 0, 0),
	deb_sum(' 2)', 1, 0, 1),
	deb_sum(' 3)', 4, 0, 4),
	deb_sum(' 4)', 0, 1, 1),
	deb_sum(' 5)', 0, 5, 5),
	deb_sum(' 6)', 1/2, 1/2, 1),
	deb_sum(' 7)', 1/2, 2/2, 3/2),
	deb_sum(' 8)', 1/2, 1, 3/2),
	deb_sum(' 9)', 1, 1/2, 3/2),
	deb_sum(' 10)', 2/2, 2/2, 2),
	deb_sum(' 11)', 2/2, 3/2, 5/2),
	deb_sum(' 12)', 3/2, 1, 5/2),
	
	write(' - OK'), nl,
	write('* SUBS'),

	deb_sub(' 1)', 0, 0, 0),
	deb_sub(' 2)', 1, 0, 1),
	deb_sub(' 3)', 4, 0, 4),
	deb_sub(' 4)', 0, 1, -1),
	deb_sub(' 5)', 0, 5, -5),
	deb_sub(' 6)', 1/2, 1/2, 0),
	deb_sub(' 7)', 1/2, 2/2, -1/2),
	deb_sub(' 8)', 1/2, 1, -1/2),
	deb_sub(' 9)', 1, 1/2, 1/2),
	deb_sub(' 10)', 2/2, 2/2, 0),
	deb_sub(' 11)', 2/2, 3/2, -1/2),
	deb_sub(' 12)', 3/2, 1, 1/2),
	
	write(' - OK'), nl,
	write('* PRODS'),

	deb_prod(' 1)', 0, 0, 0),
	deb_prod(' 2)', 1, 0, 0),
	deb_prod(' 3)', 4, 0, 0),
	deb_prod(' 4)', 0, 1, 0),
	deb_prod(' 5)', 0, 5, 0),
	deb_prod(' 6)', 1/2, 1/2, 1/4),
	deb_prod(' 7)', 1/2, 2/2, 1/2),
	deb_prod(' 8)', 1/2, 1, 1/2),
	deb_prod(' 9)', 1, 1/2, 1/2),
	deb_prod(' 10)', 2/2, 2/2, 1),
	deb_prod(' 11)', 2/2, 3/2, 3/2),
	deb_prod(' 12)', 3/2, 1, 3/2),
	
	write(' - OK'), nl,
	write('* POWERS'),

	deb_power(' 1)', 0, 0, 1),
	deb_power(' 2)', 1, 0, 1),
	deb_power(' 3)', 4, 0, 1),
	deb_power(' 4)', 0, 1, 0),
	deb_power(' 5)', 0, 5, 0),
	deb_power(' 6)', 1/2, 2, 1/4),
	deb_power(' 7)', 1/2, 1, 1/2),
	deb_power(' 8)', 2, 3, 8),
	deb_power(' 9)', 2/2, 2, 1),
	deb_power(' 10)', 2/2, 3, 1),
	deb_power(' 11)', 3/2, 1, 3/2),
	deb_power(' 12)', 3/2, 2, 9/4),
	
	write(' - OK'), nl,
	write('* ARITHMETIC EXPRESSIONS'),

	deb_arithm(' 1)', 3 + 3, 6),
	deb_arithm(' 2)', 3 - 4, -1),
	deb_arithm(' 3)', 3^4, 81),
	deb_arithm(' 4)', 2^2^2^2, 256),
	deb_arithm(' 5)', 2^2^2^2^2, 65536),
	deb_arithm(' 6)', 2^(1 + 1)^2, 16),
	deb_arithm(' 7)', 2^2^(2 + 1 - 1)^2^2, 65536),
	deb_arithm(' 8)', (1/2)^2 + 3/4, 1),
	deb_arithm(' 9)', (1/2)^0, 1),
	deb_arithm(' 10)', (1/2)^0 + 3/4, 7/4),
	deb_arithm(' 11)', 1 - (1 + 1), -1),
	deb_arithm(' 12)', 1/2, 1/2),
	
	write(' - OK'), nl,
	write('* ARITHMETIC EXPRESSIONS'),

	deb_abs(' 1)', 3, 3),
	deb_abs(' 2)', -3, 3),
	deb_abs(' 3)', 0, 0),
	deb_abs(' 4)', -0, 0),
	deb_abs(' 5)', 0, 0),
	deb_abs(' 6)', 1/2, 1/2),
	deb_abs(' 7)', -1/2, 1/2),
	deb_abs(' 8)', 0/2, 0),
	deb_abs(' 9)', -0/2, 0),
	deb_abs(' 10)', 1.4142, 1.4142),
	deb_abs(' 11)', -1.4142, 1.4142),

	write(' - OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - MONOMIAL EVALUATION
*/

deb_mon_comp(_, M, C, V, E):- monomial_comps(M, C, V, E), !.
deb_mon_comp(I, M, RC, RV, RE):-
	monomial_comps(M, C, V, E), write(I), write(' Components of '), write(M),
	write(' are ('), write(C), write(','), write(V), write(','), write(E), write(') but were expected to be '),
	write('('), write(RC), write(','), write(RV), write(','), write(RE), write(')'), false.

deb_red_mon(_, M, RES):- red_monomial(M, R), R == RES, !.
deb_red_mon(I, M, RES):- red_monomial(M, R), write(I), write(' '), write(M), write(' = '), output_text(R, RES).

deb_mon_sum(_, M1, M2, RES):- mon_sum(M1, M2, S), S == RES, !.
deb_mon_sum(I, M1, M2, RES):- mon_sum(M1, M2, S), write(I), write(' '), write(M1), write(' + '), write(M2), write(' = '), output_text(S, RES).

deb_mon_sub(_, M1, M2, RES):- mon_sub(M1, M2, S), S == RES, !.
deb_mon_sub(I, M1, M2, RES):- mon_sub(M1, M2, S), write(I), write(' '), write(M1), write(' - '), write(M2), write(' = '), output_text(S, RES).

deb_mon_prod(_, M1, M2, RES):- mon_prod(M1, M2, S), S == RES, !.
deb_mon_prod(I, M1, M2, RES):- mon_prod(M1, M2, S), write(I), write(' ('), write(M1), write(')'), write('*'), write('('), write(M2), write(')'), write(' = '), output_text(S, RES).

deb_mon_pos_coef(_, M1, RES):- monomial_positive_coefficient(M1), RES == 'YES', !.
deb_mon_pos_coef(_, _, RES):- RES == 'NO', !.
deb_mon_pos_coef(I, M1, RES):- monomial_positive_coefficient(M1), write(I), write(M1), write(' '), output_text('YES', RES).
deb_mon_pos_coef(I, M1, RES):- write(I), write(' '), write(M1), write(' -> '), output_text('NO', RES).

debug_monomials:-
	write('-- MONOMIAL EVALUATION DEBUG --'), nl,
	write('* COMPONENT EXTRACTION'),

	deb_mon_comp(' 1)', 0*x^0, 0, x, 0),
	deb_mon_comp(' 2)', 0*x^1, 0, x, 1),
	deb_mon_comp(' 3)', 3*x^0, 3, x, 0),
	deb_mon_comp(' 4)', 3*x^1, 3, x, 1),
	deb_mon_comp(' 5)', 3*x^4, 3, x, 4),
	deb_mon_comp(' 6)', (3 + 1)*x^4, 4, x, 4),
	deb_mon_comp(' 7)', -2*x^4, -2, x, 4),
	deb_mon_comp(' 8)', (-3 + 1)*x^4, -2, x, 4),
	deb_mon_comp(' 9)', (-3 + 1)*x^(4 - 4), -2, x, 0),
	deb_mon_comp(' 10)', (-3*3 + 1)*x^(4 - 4), -8, x, 0),

	write(' - OK'), nl,
	write('* MONOMIAL REDUCTION'),

	deb_red_mon(' 1)', 0*x^0, 0),
	deb_red_mon(' 2)', 0*x^1, 0),
	deb_red_mon(' 3)', 0*x^5, 0),
	deb_red_mon(' 4)', 1*x^0, 1),
	deb_red_mon(' 5)', 1*x^1, x),
	deb_red_mon(' 6)', 1*x^5, x^5),
	deb_red_mon(' 7)', 3*x^0, 3),
	deb_red_mon(' 8)', 3*x^1, 3*x),
	deb_red_mon(' 9)', 3*x^6, 3*x^6),
	deb_red_mon(' 10)', (0/1)*x^(0/1), 0),
	deb_red_mon(' 11)', (0/8)*x^(4/4), 0),
	deb_red_mon(' 12)', (0/3)*x^(20/4), 0),
	deb_red_mon(' 13)', (7/7)*x^(0/9), 1),
	deb_red_mon(' 14)', (6/6)*x^(2/2), x),
	deb_red_mon(' 15)', (1/1)*x^(5/1), x^5),
	deb_red_mon(' 16)', (27/9)*x^(0/1), 3),
	deb_red_mon(' 17)', (18/6)*x^(8/8), 3*x),
	deb_red_mon(' 18)', (15/5)*x^(36/6), 3*x^6),
	deb_red_mon(' 19)', -0*x^0, 0),
	deb_red_mon(' 20)', -(0*x^0), 0),
	deb_red_mon(' 21)', -0*x^1, 0),
	deb_red_mon(' 22)', -(0*x^1), 0),
	deb_red_mon(' 23)', -0*x^5, 0),
	deb_red_mon(' 24)', -(0*x^5), 0),
	deb_red_mon(' 25)', -1*x^0, -1),
	deb_red_mon(' 26)', -(1*x^0), -1),
	deb_red_mon(' 27)', -1*x^1, -x),
	deb_red_mon(' 28)', -(1*x^1), -x),
	deb_red_mon(' 29)', -1*x^5, -x^5),
	deb_red_mon(' 30)', -(1*x^5), -x^5),
	deb_red_mon(' 31)', -3*x^0, -3),
	deb_red_mon(' 32)', -(3*x^0), -3),
	deb_red_mon(' 33)', -3*x^1, -3*x),
	deb_red_mon(' 34)', -(3*x^1), -3*x),
	deb_red_mon(' 35)', -3*x^6, -3*x^6),
	deb_red_mon(' 36)', -(3*x^6), -3*x^6),
	deb_red_mon(' 37)', -((0/1)*x^(0/1)), 0),
	deb_red_mon(' 38)', (-0/1)*x^(0/1), 0),
	deb_red_mon(' 39)', (0/1)*x^(-0/1), 0),
	deb_red_mon(' 40)', (0/1)*x^(-(0/1)), 0),
	deb_red_mon(' 41)', -((0/8)*x^(4/4)), 0),
	deb_red_mon(' 42)', (-0/8)*x^(-4/4), 0),
	deb_red_mon(' 43)', -((0/3)*x^(20/4)), 0),
	deb_red_mon(' 44)', (-0/3)*x^(-20/4), 0),
	deb_red_mon(' 45)', -((7/7)*x^(0/9)), -1),
	deb_red_mon(' 46)', (-7/7)*x^(0/9), -1),
	deb_red_mon(' 47)', (7/7)*x^(-0/9), 1),
	deb_red_mon(' 48)', -((6/6)*x^(2/2)), -x),
	deb_red_mon(' 49)', (-6/6)*x^(2/2), -x),
	deb_red_mon(' 50)', (6/6)*x^(-2/2), x^(-1)),
	deb_red_mon(' 51)', -((1/1)*x^(5/1)), -x^5),
	deb_red_mon(' 52)', (-1/1)*x^(5/1), -x^5),
	deb_red_mon(' 53)', (1/1)*x^(-5/1), x^(-5)),
	deb_red_mon(' 54)', -((27/9)*x^(0/1)), -3),
	deb_red_mon(' 56)', (-27/9)*x^(0/1), -3),
	deb_red_mon(' 57)', (27/9)*x^(-0/1), 3),
	deb_red_mon(' 58)', -((18/6)*x^(8/8)), -3*x),
	deb_red_mon(' 59)', (-18/6)*x^(8/8), -3*x),
	deb_red_mon(' 60)', (18/6)*x^(-8/7), 3*x^(-8/7)),
	deb_red_mon(' 61)', -((15/5)*x^(36/8)), -3*x^(9/2)),
	deb_red_mon(' 62)', (-15/5)*x^(36/8), -3*x^(9/2)),
	deb_red_mon(' 63)', (15/5)*x^(-36/8), 3*x^(-9/2)),
	deb_red_mon(' 64)', -(1/2*x), -1/2*x),
	deb_red_mon(' 65)', -1/2*x, -1/2*x),
	deb_red_mon(' 66)', (-1/2)*x, -1/2*x),

	write(' - OK'), nl,
	write('* MONOMIAL SUM'),

	deb_mon_sum(' 1)', 3*x, 2*x, 5*x),
	deb_mon_sum(' 2)', 3*x, 2*x^0, 3*x + 2),
	deb_mon_sum(' 3)', 3*x, 2*x^1, 5*x),
	deb_mon_sum(' 4)', 3*x^0, 2*x, 3 + 2*x),
	deb_mon_sum(' 5)', 3*x^1, 2*x, 5*x),
	deb_mon_sum(' 6)', 3*x^2, 2*x, 3*x^2 + 2*x),
	deb_mon_sum(' 7)', 3*x^2, 2*x^2, 5*x^2),
	deb_mon_sum(' 8)', 3*x^0, 2*x^0, 5),
	deb_mon_sum(' 9)', 3*x^0, 0*x^2, 3),
	deb_mon_sum(' 10)', 0*x^1, 0*x^2, 0),
	deb_mon_sum(' 11)', 0, 0*x^2, 0),
	deb_mon_sum(' 12)', 0, 0, 0),

	write(' - OK'), nl,
	write('* MONOMIAL SUB'),

	deb_mon_sub(' 1)', 0, x, -x),
	deb_mon_sub(' 2)', 3*x, 2*x, x),
	deb_mon_sub(' 3)', 3*x, 2*x^0, 3*x - 2),
	deb_mon_sub(' 4)', 3*x, 2*x^1, x),
	deb_mon_sub(' 5)', 3*x^0, 2*x, 3 - 2*x),
	deb_mon_sub(' 6)', 3*x^1, 2*x, x),
	deb_mon_sub(' 7)', 3*x^2, 2*x, 3*x^2 - 2*x),
	deb_mon_sub(' 8)', 3*x^2, 2*x^2, x^2),
	deb_mon_sub(' 9)', 3*x^0, 2*x^0, 1),
	deb_mon_sub(' 10)', 3*x^0, 0*x^2, 3),
	deb_mon_sub(' 11)', 0*x^1, 0*x^2, 0),
	deb_mon_sub(' 12)', 0, 0*x^2, 0),
	deb_mon_sub(' 13)', 0, 0, 0),

	write(' - OK'), nl,
	write('* MONOMIAL PROD'),

	deb_mon_prod(' 1)', 3*x, 2*x, 6*x^2),
	deb_mon_prod(' 2)', 3*x, 2*x^0, 6*x),
	deb_mon_prod(' 3)', 3*x, 2*x^1, 6*x^2),
	deb_mon_prod(' 4)', 3*x^0, 2*x, 6*x),
	deb_mon_prod(' 5)', 3*x^1, 2*x, 6*x^2),
	deb_mon_prod(' 6)', 3*x^2, 2*x, 6*x^3),
	deb_mon_prod(' 7)', 3*x^2, 2*x^2, 6*x^4),
	deb_mon_prod(' 8)', 3*x^0, 2*x^0, 6),
	deb_mon_prod(' 9)', 3*x^0, 0*x^2, 0),
	deb_mon_prod(' 10)', 0*x^1, 0*x^2, 0),
	deb_mon_prod(' 11)', 0, 0*x^2, 0),
	deb_mon_prod(' 12)', 0, 0, 0),

	write(' - OK'), nl,
	write('* MONOMIAL POSITIVE COEFFICIENT'),

	deb_mon_pos_coef(' 1)', 3*x, 'YES'),
	deb_mon_pos_coef(' 2)', -3*x, 'NO'),
	deb_mon_pos_coef(' 3)', -x, 'NO'),
	deb_mon_pos_coef(' 4)', x, 'YES'),

	write(' - OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - POLYNOMIAL EVALUATION
*/

deb_poly_sum(_, P, RES):- polynomial_sum(P, R), polynomial_eq(R, RES), !.
deb_poly_sum(I, P, RES):- polynomial_sum(P, R), write(I), write(' '), write(P), write(' = '), output_text(R, RES).

deb_poly_prod(_, P1, P2, RES):- polynomial_prod(P1, P2, R), polynomial_eq(R, RES), !.
deb_poly_prod(I, P1, P2, RES):- polynomial_prod(P1, P2, R), write(I), write(' ('), write(P1), write(')*('), write(P2), write(') = '), output_text(R, RES).

deb_poly_pow(_, P, N, RES):- polynomial_power(P, N, R), polynomial_eq(R, RES), !.
deb_poly_pow(I, P, N, RES):- polynomial_power(P, N, R), write(I), write('( '), write(P), write(')^'), write(N), write(' = '), output_text(R, RES).

deb_poly_eval(_, E, RES):- polynomial_evaluation(E, R), polynomial_eq(R, RES), !.
deb_poly_eval(I, E, RES):- polynomial_evaluation(E, R), write(I), write(' '), write(E), write(' = '), output_text(R, RES).

deb_poly_first_mon(_, E, RES1, RES2):- polynomial_first_monomial(E, R1, R2), polynomial_eq(R1, RES1), polynomial_eq(R2, RES2), !.
deb_poly_first_mon(I, E, RES1, RES2):- polynomial_first_monomial(E, R1, R2), write(I), write('' ), write(E), write(' -> '), output_text( (R1, R2), (RES1, RES2)).

deb_poly_last_mon(_, E, RES1, RES2):- polynomial_last_monomial(E, R1, R2), polynomial_eq(R1, RES1), polynomial_eq(R2, RES2), !.
deb_poly_last_mon(I, E, RES1, RES2):- polynomial_last_monomial(E, R1, R2), write(I), write('' ), write(E), write(' -> '), output_text( (R1, R2), (RES1, RES2)).

deb_poly_pad(_, L, RES):- padded_poly_mons_incr(L, R), R == RES, !.
deb_poly_pad(I, L, RES):- padded_poly_mons_incr(L, R), write(I), write(' '), write(L), write(' -> '), output_text(R, RES).

deb_poly_roots(_, P, RES):- polynomial_evaluation(P, E), integer_roots_polynomial(E, R), msort(R, RS), msort(RES, RESS), RS == RESS, !.
deb_poly_roots(I, P, RES):- polynomial_evaluation(P, E), integer_roots_polynomial(E, R), write(I), write(' '), write(P), write(' -> '), output_text(R, RES).

debug_polynomials:-
	write('-- POLYNOMIAL EVALUATION DEBUG --'), nl,
	write('* POLYNOMIAL REDUCTION'),

	deb_poly_sum(' 1)', 0*x^0, 0),
	deb_poly_sum(' 2)', 0 + x + 0, x),
	deb_poly_sum(' 3)', 0 + x^2 + x^(1 + 1), 2*x^2),
	deb_poly_sum(' 4)', x^2 - 2*x^(3 - 1), -x^2),
	deb_poly_sum(' 5)', x^2 - 2*x^(3 - 1), -x^2),
	deb_poly_sum(' 6)', (1/2)*x - (1/2)*x, 0),
	deb_poly_sum(' 7)', (1/2)*x + x^2 - (1/2)*x, x^2),
	deb_poly_sum(' 8)', (1/2)*x - (3/2)*x, -x),
	deb_poly_sum(' 9)', 0 + (1/2)*x + (3/2)*x, 2*x),
	deb_poly_sum(' 10)', 0 + (1/2)*x - 0 + (3/2)*x, 2*x),
	deb_poly_sum(' 11)', x^2 + (1/2)*x - 0 + (3/2)*x, 2*x + x^2),
	deb_poly_sum(' 12)', x - x^2 + x^3 - x^4 + x^5, x - x^2 + x^3 - x^4 + x^5),
	deb_poly_sum(' 13)', (1/3)*x^4 + (1/2)*x^3 + (1/6)*x^2 - (1/6)*x^3 + (1/4)*x^2 - (1/12)*x - (1/12)*x^2 + (1/12)*x - x^3, (1/3)*x^2 - (2/3)*x^3 + (1/3)*x^4),

	write(' - OK'), nl,
	write('* POLYNOMIAL PRODUCT'),

	deb_poly_prod(' 1)', x, x, x^2),
	deb_poly_prod(' 2)', x^2, x, x^3),
	deb_poly_prod(' 3)', x + 1, x, x^2 + x),
	deb_poly_prod(' 4)', x, x + 1, x^2 + x),
	deb_poly_prod(' 5)', x + 1, x + 1, x^2 + 2*x + 1),
	deb_poly_prod(' 5)', x^2 + 1, x + 1, x^3 + x^2 + x + 1),
	deb_poly_prod(' 6)', x^2 + 2, 6*x + 1, 6*x^3 + x^2 + 12*x + 2),
	deb_poly_prod(' 7)', x^2 + 2*x^2, -6*x - 1, -18*x^3 - 3*x^2),
	deb_poly_prod(' 8)', x^2 + 0, -6*x - 1, -6*x^3 - x^2),
	deb_poly_prod(' 9)', -1 + x^2 + 1, -6*x + 1, -6*x^3 + x^2),
	deb_poly_prod(' 10)', -1 + x^2 + 1, -1 + x^2 + 1, x^4),
	deb_poly_prod(' 11)', 2*x^2 - 3*x^3 + 30, 2*x^2 - 3*x^3 + 30, 9*x^6 - 12*x^5 + 4*x^4 - 180*x^3 + 120*x^2 + 900),

	write(' - OK'), nl,
	write('* POLYNOMIAL POWER'),
	
	deb_poly_pow(' 1)', x, 0, 1),
	deb_poly_pow(' 2)', x + 1, 0, 1),
	deb_poly_pow(' 3)', x + 1, 1, x + 1),
	deb_poly_pow(' 4)', x - 1, 1, x - 1),
	deb_poly_pow(' 5)', x - 1, 2, x^2 - 2*x + 1),
	deb_poly_pow(' 6)', x + 1, 2, x^2 + 2*x + 1),
	deb_poly_pow(' 7)', 2*x + 1, 2, 4*x^2 + 4*x + 1),
	deb_poly_pow(' 8)', 2*x^2, 2, 4*x^4),
	deb_poly_pow(' 9)', x^2, 2, x^4),
	deb_poly_pow(' 10)', x + 1, 3, x^3 + 3*x^2 + 3*x + 1),
	deb_poly_pow(' 11)', 2*x^2 - 3*x^3 + 30, 1, 2*x^2 - 3*x^3 + 30),
	deb_poly_pow(' 12)', 2*x^2 - 3*x^3 + 30, 2, 9*x^6 - 12*x^5 + 4*x^4 - 180*x^3 + 120*x^2 + 900),
	deb_poly_pow(' 13)', 2*x^2 - 3*x^3 + 30, 3, -27*x^9 + 54*x^8 - 36*x^7 + 818*x^6 - 1080*x^5 + 360*x^4 - 8100*x^3 + 5400*x^2 + 27000),
	deb_poly_pow(' 14)', 2*x^2 - 3*x^3 + 30, 4, 81*x^12 - 216*x^11 + 216*x^10 - 3336*x^9 + 6496*x^8 - 4320*x^7 + 49560*x^6 - 64800*x^5 + 21600*x^4 - 324000*x^3 + 216000*x^2 + 810000),
	deb_poly_pow(' 15)', 2*x^2 - 3*x^3 + 30, 5, -243*x^15 + 810*x^14 - 1080*x^13 + 12870*x^12 - 32640*x^11 + 32432*x^10 - 257400*x^9 + 488400*x^8 - 324000*x^7 + 2502000*x^6 - 3240000*x^5 + 1080000*x^4 - 12150000*x^3 + 8100000*x^2 + 24300000),

	write(' - OK'), nl,
	write('* POLYNOMIAL EVALUATION'),

	deb_poly_eval(' 1)', x + x - 2, 2*x - 2),
	deb_poly_eval(' 2)', x + x^2 - 2, x^2 + x - 2),
	deb_poly_eval(' 3)', x*x^2 - 2, x^3 - 2),
	deb_poly_eval(' 4)', (x + 1)^2 - 1, x^2 + 2*x),
	deb_poly_eval(' 5)', x*(x + 1)^2 - 1, x^3 + 2*x^2 + x - 1),
	deb_poly_eval(' 6)', x*(x + 1)^2 - x, x^3 + 2*x^2),
	deb_poly_eval(' 7)', (1/8)*x*((x + 1)^2)*(2*x + 1) - (1/16)*x*(x + 1)*(2*x + 1) - (1/16)*x*(x + 1), 1/4*x^4+1/2*x^3+1/4*x^2),
	deb_poly_eval(' 8)', (-1/8)*(n^2 + n)^2 + (1/12)*n*(n - 2)*(n + 1)*(2*n + 1) + (1/4)*n*(n - 1)*(n + 1), 1/24*n^4-1/12*n^3-13/24*n^2-5/12*n),

	write(' - OK'), nl,
	write('* POLYNOMIAL\' FIRST AND LAST MONOMIALS'),
	deb_poly_first_mon(' 1.1)', 3*x - 2*x^2, 3*x, -2*x^2),
	deb_poly_first_mon(' 1.2)', 3*x + 2*x^2, 3*x, 2*x^2),
	deb_poly_first_mon(' 1.3)', 3*x - 2*x^3 + 4*x^4, 3*x, -2*x^3 + 4*x^4),
	deb_poly_first_mon(' 1.4)', 3*x + 2*x^2 + 4*x^3, 3*x, 2*x^2 + 4*x^3),
	
	deb_poly_last_mon(' 2.1)', 3*x - 2*x^2, 3*x, -2*x^2),
	deb_poly_last_mon(' 2.2)', 3*x + 2*x^2, 3*x, 2*x^2),
	deb_poly_last_mon(' 2.3)', 3*x - 2*x^3 + 4*x^4, 3*x - 2*x^3, 4*x^4),
	deb_poly_last_mon(' 2.4)', 3*x + 2*x^2 + 4*x^3, 3*x + 2*x^2, 4*x^3),

	write(' - OK'), nl,
	write('* POLYNOMIAL PADDING'),

	deb_poly_pad(' 1)', [2], [2]),
	deb_poly_pad(' 2)', [x], [x, 0]),
	deb_poly_pad(' 3)', [x, 3], [x, 3]),
	deb_poly_pad(' 4)', [x^2], [x^2, 0, 0]),
	deb_poly_pad(' 5)', [x^2, 3], [x^2, 0, 3]),
	deb_poly_pad(' 6)', [x^3, x], [x^3, 0, x, 0]),
	deb_poly_pad(' 7)', [x^4, x^3, x], [x^4, x^3, 0, x, 0]),
	deb_poly_pad(' 8)', [x^4, x^2, 1], [x^4, 0, x^2, 0, 1]),
	deb_poly_pad(' 9)', [x^3], [x^3, 0, 0, 0]),
	deb_poly_pad(' 10)', [x^4], [x^4, 0, 0, 0, 0]),
	deb_poly_pad(' 11)', [x^6, x^4], [x^6, 0, x^4, 0, 0, 0, 0]),

	write(' - OK'), nl,
	write('* POLYNOMIAL ROOTS'),

	deb_poly_roots(' 1)', (x + 1)^2, [-1, -1]),
	deb_poly_roots(' 2)', ((x + 1)^2)*(x - 3), [-1, -1, 3]),
	deb_poly_roots(' 3)', ((x + 1)^2)*(x - 3)^2, [-1, -1, 3, 3]),
	deb_poly_roots(' 4)', ((x + 1)^2)*(x - 3)*(x + 3), [-1, -1, -3, 3]),
	deb_poly_roots(' 5)', (x + 1)*(x - 1)*(x - 3)*(x + 3), [-1, 1, -3, 3]),

	write(' - OK'), nl,
	nl, true.
