:-ensure_loaded(core).
:-ensure_loaded(algorithms/power_sums).

debug:-
	debug_integer_algs,
	debug_lists,
	debug_numbers,
	debug_arithmetic_evaluation,
	debug_monomials,
	debug_polynomials,
	debug_power_sums.

main:- debug, halt.
main:- nl, write('ERROR'), nl, halt.

output_text(OBT, EXP):- write(OBT), write(' | Not correct - Expected to see: '), write(EXP), nl, false.

all_to_string([space], ' '):- !.
all_to_string([X], S):- term_to_atom(X, S), !.
all_to_string([space|L], S):- all_to_string(L, LS), atom_concat(' ', LS, S), !.
all_to_string([X|L], S):- term_to_atom(X, AX), all_to_string(L, LS), atom_concat(AX, LS, S), !.

/*
-----------------------------
DEBUG - INTEGER ALGORITHMS
*/

deb_divisors(_, N, RES):- divisors(N, R), R == RES, !.
deb_divisors(I, N, RES):-
	divisors(N, R),
	write(I), write(' Divisors( '), write(N), write(' ): '), output_text(R, RES).

deb_gcd(_, A, B, RES):- gcd(A, B, R), R == RES, !.
deb_gcd(I, A, B, RES):-
	gcd(A, B, R),
	write(I), write(' gcd('), write(A), write(','), write(B), write(')= '), output_text(R, RES).

debug_integer_algs:-
	write('-- INTEGER ALGORITHMS DEBUG --'), nl,
	write('* DIVISORS'),

	deb_divisors(' 1)', 1, [1,-1]),
	deb_divisors(' 2)', 2, [1,-1,2,-2]),
	deb_divisors(' 3)', 3, [1,-1,3,-3]),
	deb_divisors(' 4)', 6, [1,-1,2,-2,3,-3,6,-6]),
	deb_divisors(' 5)', 8, [1,-1,2,-2,4,-4,8,-8]),
	deb_divisors(' 6)', 12, [1,-1,2,-2,3,-3,4,-4,6,-6,12,-12]),
	deb_divisors(' 7)', 20, [1,-1,2,-2,4,-4,5,-5,10,-10,20,-20]),
	deb_divisors(' 8)', 24, [1,-1,2,-2,3,-3,4,-4,6,-6,8,-8,12,-12,24,-24]),
	deb_divisors(' 9)', 32, [1,-1,2,-2,4,-4,8,-8,16,-16,32,-32]),
	deb_divisors(' 10)', 576, [1,-1,2,-2,3,-3,4,-4,6,-6,8,-8,9,-9,12,-12,16,-16,18,-18,24,-24,32,-32,36,-36,48,-48,64,-64,72,-72,96,-96,144,-144,192,-192,288,-288,576,-576]),
	deb_divisors(' 11)', -1, [1,-1]),
	deb_divisors(' 12)', -2, [1,-1,2,-2]),
	deb_divisors(' 13)', -3, [1,-1,3,-3]),
	deb_divisors(' 14)', -6, [1,-1,2,-2,3,-3,6,-6]),
	deb_divisors(' 15)', -8, [1,-1,2,-2,4,-4,8,-8]),
	deb_divisors(' 16)', -12, [1,-1,2,-2,3,-3,4,-4,6,-6,12,-12]),
	deb_divisors(' 17)', -20, [1,-1,2,-2,4,-4,5,-5,10,-10,20,-20]),
	deb_divisors(' 18)', -24, [1,-1,2,-2,3,-3,4,-4,6,-6,8,-8,12,-12,24,-24]),
	deb_divisors(' 19)', -32, [1,-1,2,-2,4,-4,8,-8,16,-16,32,-32]),
	deb_divisors(' 20)', -576, [1,-1,2,-2,3,-3,4,-4,6,-6,8,-8,9,-9,12,-12,16,-16,18,-18,24,-24,32,-32,36,-36,48,-48,64,-64,72,-72,96,-96,144,-144,192,-192,288,-288,576,-576]),

	write(' -> OK'), nl,
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
	deb_gcd(' 16)', 247, -270, 1),
	deb_gcd(' 17)', -247, 270, 1),
	deb_gcd(' 18)', -247, -270, 1),

	write(' -> OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - LIST OPERATIONS
*/

deb_insertion_sort(_, L):- isort(L, R), msort(L, RES), R == RES, !.
deb_insertion_sort(I, L):- isort(L, R), msort(L, RES), write(I), write(L), write(' -> '), output_text(R, RES).

deb_how_many(_, L, RES):- how_many(L, RES), !.
deb_how_many(I, L, RES):-
	how_many(L, R),
	write(I), write(' Count of '), write(L), write(': '), output_text(R, RES).

deb_cart_prod(_, A, B, RES):- cartesian_product(A, B, RES), !.
deb_cart_prod(I, A, B, RES):-
	cartesian_product(A, B, R),
	write(I), write(' '), write(A), write(' x '), write(B), write(' = '), output_text(R, RES).

dumb_sum__(A, B, E):- E is A + B.

deb_cart_prod_by(_, A, B, RES):- cartesian_product_by(dumb_sum__, A, B, RES), !.
deb_cart_prod_by(I, A, B, RES):-
	cartesian_product_by(dumb_sum__, A, B, R),
	write(I), write(' '), write(A), write(' (x) '), write(B), write(' = '), output_text(R, RES).

debug_lists:-
	write('-- LISTS OPERATIONS DEBUG --'), nl,
	write('* SORTING ALGORITHMS - INSERTION SORT'),

	deb_insertion_sort(' 1) ', [3,2,1]),
	deb_insertion_sort(' 2) ', [123,4,46,7,578,67,8567,58,21,23,4,245,3,2,1]),
	deb_insertion_sort(' 3) ', [-4,8567,25,123,4,46,7,-8,578,67,-4,8567,58,21,-7,23,4,245,3,2,1,8567]),

	write(' -> OK'), nl,
	write('* COUNTING HOW MANY'),

	deb_how_many(' 1)', [1], [[1,1]]),
	deb_how_many(' 2)', [1,1], [[1,2]]),
	deb_how_many(' 3)', [1,1,1], [[1,3]]),
	deb_how_many(' 4)', [2,1,1,1], [[1,3],[2,1]]),
	deb_how_many(' 5)', [2,1,1,2], [[1,2],[2,2]]),
	deb_how_many(' 6)', [3,1,1,2], [[1,2],[2,1],[3,1]]),
	deb_how_many(' 7)', [3,1,1,2,7], [[1,2],[2,1],[3,1],[7,1]]),
	deb_how_many(' 8)', [3,1,1,2,1], [[1,3],[2,1],[3,1]]),
	deb_how_many(' 9)', [3,1,3,2,1], [[1,2],[2,1],[3,2]]),
	deb_how_many(' 10)', [4,1,3,2,1], [[1,2],[2,1],[3,1],[4,1]]),

	write(' -> OK'), nl,
	write('* CARTESIAN PRODUCT'),

	deb_cart_prod(' 1)', [], [], []),
	deb_cart_prod(' 2)', [1], [], []),
	deb_cart_prod(' 3)', [1,2], [], []),
	deb_cart_prod(' 4)', [], [1], []),
	deb_cart_prod(' 5)', [], [1,2], []),
	deb_cart_prod(' 6)', [1], [1], [[1,1]]),
	deb_cart_prod(' 7)', [1,2], [1], [[1,1],[2,1]]),
	deb_cart_prod(' 8)', [1], [1,2], [[1,1],[1,2]]),
	deb_cart_prod(' 9)', [1,3], [1,2], [[1,1],[1,2],[3,1],[3,2]]),
	deb_cart_prod(' 10)', [1,2], [1,3], [[1,1],[1,3],[2,1],[2,3]]),

	write(' -> OK'), nl,
	write('* CARTESIAN PRODUCT BY'),

	deb_cart_prod_by(' 1)', [], [], []),
	deb_cart_prod_by(' 2)', [1], [], []),
	deb_cart_prod_by(' 3)', [1,2], [], []),
	deb_cart_prod_by(' 4)', [], [1], []),
	deb_cart_prod_by(' 5)', [], [1,2], []),
	deb_cart_prod_by(' 6)', [1], [1], [2]),
	deb_cart_prod_by(' 7)', [1,2], [1], [2,3]),
	deb_cart_prod_by(' 8)', [1], [1,2], [2,3]),
	deb_cart_prod_by(' 9)', [1,3], [1,2], [2,3,4,5]),
	deb_cart_prod_by(' 10)', [1,2], [1,3], [2,4,3,5]),

	write(' -> OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - NUMBERS
*/

deb_red_frac(_, F, RES):- red_frac(F, RES), !.
deb_red_frac(I, F, RES):- red_frac(F, R), write(I), write(' '), write(F), write(' = '), output_text(R, RES).

deb_rat_gcd(_, F1, F2, RES1, RES2, RES3):- rational_gcd_rel(F1, F2, RES1, RES2, RES3), !.
deb_rat_gcd(I, F1, F2, RES1, RES2, RES3):- rational_gcd_rel(F1, F2, R1, R2, R3),
	term_to_atom(R1, AR1), term_to_atom(R2, AR2), term_to_atom(R3, AR3),
	term_to_atom(RES1, ARES1), term_to_atom(RES2, ARES2), term_to_atom(RES3, ARES3),

	write(I), write(' '), write(F1), write(' + '), write(F2), write(' = '),
	atom_concat(AR1, '*(', S1), atom_concat(S1, AR2, S2),
	atom_concat(S2, ' + ', S3), atom_concat(S3, AR3, S4), atom_concat(S4, ')', R),
	atom_concat(ARES1, '*(', SES1), atom_concat(SES1, ARES2, SES2),
	atom_concat(SES2, ' + ', SES3), atom_concat(SES3, ARES3, SES4),
	atom_concat(SES4, ')', RES), output_text(R, RES).

debug_numbers:-
	write('-- NUMBERS DEBUG --'), nl,
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

	write(' -> OK'), nl,
	write('* RATIONAL GREATEST COMMON DIVISOR'),

	deb_rat_gcd(' 1)', 1, 2, 1, 1, 2),
	deb_rat_gcd(' 2)', 2, 2, 2, 1, 1),
	deb_rat_gcd(' 3)', 4, 2, 2, 2, 1),
	deb_rat_gcd(' 4)', 4, 4, 4, 1, 1),
	deb_rat_gcd(' 5)', 7, 3, 1, 7, 3),
	deb_rat_gcd(' 6)', 18, 12, 6, 3, 2),
	deb_rat_gcd(' 7)', 15, 12, 3, 5, 4),
	deb_rat_gcd(' 8)', 1/3, 1/6, 1/3, 1/1, 1/2),
	deb_rat_gcd(' 9)', -1/3, 1/6, 1/3, -1/1, 1/2),
	deb_rat_gcd(' 10)', 1/3, -1/6, 1/3, 1/1, -1/2),
	deb_rat_gcd(' 11)', -1/3, -1/6, 1/3, -1/1, -1/2),
	deb_rat_gcd(' 12)', 5, 5/3, 5, 1, 1/3),
	deb_rat_gcd(' 13)', 2, 1/2, 1, 2, 1/2),
	deb_rat_gcd(' 14)', 2, -1/2, 1, 2, -1/2),

	write(' -> OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - ARITHMETIC EVALUATION
*/

deb_sum(_, A, B, RES):- eval_sum(A, B, RES), !.
deb_sum(I, A, B, RES):-
	eval_sum(A, B, R),
	write(I), write(' '), write(A), write(' + '), write(B), write(' = '),
	output_text(R, RES).

deb_sub(_, A, B, RES):- eval_sub(A, B, RES), !.
deb_sub(I, A, B, RES):-
	eval_sub(A, B, R),
	write(I), write(' '), write(A), write(' - '), write(B), write(' = '),
	output_text(R, RES).

deb_prod(_, A, B, RES):- eval_prod(A, B, RES), !.
deb_prod(I, A, B, RES):-
	eval_sub(A, B, R),
	write(I), write(' '), write(A), write(' * '), write(B), write(' = '),
	output_text(R, RES).

deb_power(_, A, B, RES):- eval_pow(A, B, RES), !.
deb_power(I, A, B, RES):-
	eval_pow(A, B, R),
	write(I), write(' '), write(A), write('^'), write(B), write(' = '),
	output_text(R, RES).

deb_arithm(_, E, RES):- arithmetic_eval(E, RES), !.
deb_arithm(I, E, RES):-
	arithmetic_eval(E, R),
	write(I), write(' '), write(E), write(' = '), output_text(R, RES).

deb_abs(_, E, RES):- abs_real(E, RES), !.
deb_abs(I, E, RES):-
	abs_real(E, R),
	write(I), write(' |'), write(E), write('| = '), output_text(R, RES), !.

debug_arithmetic_evaluation:-
	write('-- ARITHMETIC EVALUATION DEBUG --'), nl,
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

	write(' -> OK'), nl,
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
	deb_sub(' 13)', 0, 1/25, -1/25),

	write(' -> OK'), nl,
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

	write(' -> OK'), nl,
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

	write(' -> OK'), nl,
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

	write(' -> OK'), nl,
	write('* ABSOLUTE VALUES'),

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

	write(' -> OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - MONOMIAL EVALUATION
*/

deb_mon_comp(_, M, C, V, E):- monomial_comps(M, C, V, E), !.
deb_mon_comp(I, M, RC, RV, RE):-
	monomial_comps(M, C, V, E), write(I), write(' Components of '),
	write(M), write(' are ('), write(C), write(','), write(V), write(','),
	write(E), write(') but were expected to be '), write('('), write(RC),
	write(','), write(RV), write(','), write(RE), write(')'), false.

deb_red_mon(_, M, RES):- red_monomial(M, RES), !.
deb_red_mon(I, M, RES):-
	red_monomial(M, R),
	write(I), write(' '), write(M), write(' = '), output_text(R, RES).

deb_mon_sum(_, M1, M2, RES):- mon_sum(M1, M2, RES), !.
deb_mon_sum(I, M1, M2, RES):-
	mon_sum(M1, M2, S),
	write(I), write(' '), write(M1), write(' + '), write(M2), write(' = '),
	output_text(S, RES).

deb_mon_sub(_, M1, M2, RES):- mon_sub(M1, M2, RES), !.
deb_mon_sub(I, M1, M2, RES):-
	mon_sub(M1, M2, S),
	write(I), write(' '), write(M1), write(' - '), write(M2), write(' = '),
	output_text(S, RES).

deb_mon_prod(_, M1, M2, RES):- mon_prod(M1, M2, RES), !.
deb_mon_prod(I, M1, M2, RES):-
	mon_prod(M1, M2, S),
	write(I), write(' ('), write(M1), write(')'), write('*'), write('('),
	write(M2), write(')'), write(' = '), output_text(S, RES).

deb_mon_pos_coef(_, M1, RES):- monomial_positive_coefficient(M1), RES == 'YES', !.
deb_mon_pos_coef(_, _, RES):- RES == 'NO', !.
deb_mon_pos_coef(I, M1, RES):- monomial_positive_coefficient(M1), write(I),
	write(M1), write(' '), output_text('YES', RES).
deb_mon_pos_coef(I, M1, RES):- write(I), write(' '), write(M1), write(' -> '),
	output_text('NO', RES).

deb_mon_eval(_, M, V, VAL, RES):- monomial_evaluation(VAL, V, M, RES), !.
deb_mon_eval(I, M, V, VAL, RES):-
	monomial_evaluation(VAL, V, M, R),
	write(I), write(' M('), write(V), write(')='), write(M), write('('), write(VAL), write(') = '),
	output_text(R, RES).

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

	write(' -> OK'), nl,
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

	write(' -> OK'), nl,
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

	write(' -> OK'), nl,
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
	deb_mon_sub(' 14)', 0, 1/24, -1/24),

	write(' -> OK'), nl,
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

	write(' -> OK'), nl,
	write('* MONOMIAL POSITIVE COEFFICIENT'),

	deb_mon_pos_coef(' 1)', 3*x, 'YES'),
	deb_mon_pos_coef(' 2)', -3*x, 'NO'),
	deb_mon_pos_coef(' 3)', -x, 'NO'),
	deb_mon_pos_coef(' 4)', x, 'YES'),

	write(' -> OK'), nl,
	write('* MONOMIAL EVALUATION'),

	deb_mon_eval(' 1)',	   1, x, 0, 1),
	deb_mon_eval(' 2)',        1, x, 1, 1),
	deb_mon_eval(' 3)',        1, x, 100, 1),
	deb_mon_eval(' 4)',       -1, x, 100, -1),
	deb_mon_eval(' 5)',       -1, x, -2, -1),
	deb_mon_eval(' 6)',       -x, x, -2, 2),
	deb_mon_eval(' 7)',        x, x, 2, 2),
	deb_mon_eval(' 8)',      x^2, x, 2, 4),
	deb_mon_eval(' 9)',      3*x, x, 2, 6),
	deb_mon_eval(' 10)',   3*x^2, x, 2, 12),
	deb_mon_eval(' 11)',   9*x^2, x, 2, 36),
	deb_mon_eval(' 12)',    -9*x, x, 2, -18),
	deb_mon_eval(' 13)', -10*x^3, x, 0, 0),
	deb_mon_eval(' 14)',     x^3, x, 0, 0),
	deb_mon_eval(' 15)',       0, x, 11, 0),
	deb_mon_eval(' 16)',	 3*x, i, 11, 3*x),
	deb_mon_eval(' 17)',       0, i, 11, 0),
	deb_mon_eval(' 18)',     3*i, i, 3, 9),

	write(' -> OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - POLYNOMIAL EVALUATION
*/

deb_poly_list_sum(_, P1, P2, RES):-
	polynomial_from_list_sum_list(P1, P2, R), polynomial_list_eq(RES, R), !.
deb_poly_list_sum(I, P1, P2, RES):-
	polynomial_from_list_sum_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' + '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_sorted_list_sum(_, P1, P2, RES):-
	polynomial_from_list_sum_sorted_list(P1, P2, R), polynomial_list_eq(RES, R), !.
deb_poly_sorted_list_sum(I, P1, P2, RES):-
	polynomial_from_list_sum_sorted_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' + '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_list_sub(_, P1, P2, RES):-
	polynomial_from_list_sub_list(P1, P2, R), polynomial_list_eq(RES, R), !.
deb_poly_list_sub(I, P1, P2, RES):-
	polynomial_from_list_sub_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' - '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_sorted_list_sub(_, P1, P2, RES):-
	polynomial_from_list_sub_sorted_list(P1, P2, R), polynomial_list_eq(RES, R), !.
deb_poly_sorted_list_sub(I, P1, P2, RES):-
	polynomial_from_list_sub_sorted_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' - '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_list_prod(_, P1, P2, RES):-
	polynomial_from_list_prod_list(P1, P2, R), polynomial_list_eq(RES, R), !.
deb_poly_list_prod(I, P1, P2, RES):-
	polynomial_from_list_prod_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' * '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_sorted_list_prod(_, P1, P2):-
	polynomial_from_list_prod_sorted_list(P1, P2, R1), polynomial_from_list_prod_list(P1, P2, R2),
	polynomial_list_eq(R1, R2), !.
deb_poly_sorted_list_prod(I, P1, P2):-
	polynomial_from_list_prod_sorted_list(P1, P2, R1),
	polynomial_from_list_prod_list(P1, P2, R2), write(I), write(' '), write(P1),
	write(' * '), write(P2), write(' = '), output_text(R1, R2).

deb_poly_sum(_, P, RES):- polynomial_sum(P, R), polynomial_eq(R, RES), !.
deb_poly_sum(I, P, RES):-
	polynomial_sum(P, R),
	write(I), write(' '), write(P), write(' = '), output_text(R, RES).

deb_poly_prod(_, P1, P2, RES):-
	polynomial_prod(P1, P2, R), polynomial_eq(R, RES), !.
deb_poly_prod(I, P1, P2, RES):-
	polynomial_prod(P1, P2, R),
	write(I), write(' ('), write(P1), write(')*('), write(P2), write(') = '),
	output_text(R, RES).

deb_poly_pow(_, P, N, RES):- polynomial_power(P, N, R), polynomial_eq(R, RES), !.
deb_poly_pow(I, P, N, RES):-
	polynomial_power(P, N, R),
	write(I), write(' ('), write(P), write(')^'), write(N), write(' = '),
	output_text(R, RES).

deb_poly_eval(_, E, RES):- polynomial_evaluation(E, R), polynomial_eq(R, RES), !.
deb_poly_eval(I, E, RES):-
	polynomial_evaluation(E, R),
	write(I), write(' '), write(E), write(' = '), output_text(R, RES).

deb_poly_first_mon(_, E, RES1, RES2):-
	polynomial_first_monomial(E, R1, R2), polynomial_eq(R1, RES1),
	polynomial_eq(R2, RES2), !.
deb_poly_first_mon(I, E, RES1, RES2):-
	polynomial_first_monomial(E, R1, R2),
	write(I), write('' ), write(E), write(' -> '),
	output_text( (R1, R2), (RES1, RES2)).

deb_poly_last_mon(_, E, RES1, RES2):-
	polynomial_last_monomial(E, R1, R2), polynomial_eq(R1, RES1),
	polynomial_eq(R2, RES2), !.
deb_poly_last_mon(I, E, RES1, RES2):-
	polynomial_last_monomial(E, R1, R2),
	write(I), write('' ), write(E), write(' -> '),
	output_text( (R1, R2), (RES1, RES2)).

deb_poly_pad(_, L, RES):- padded_poly_mons_decr(L, R), R == RES, !.
deb_poly_pad(I, L, RES):-
	padded_poly_mons_decr(L, R),
	write(I), write(' '), write(L), write(' -> '), output_text(R, RES).

deb_pretty_polynomial_roots(_, R, RES):-
	pretty_polynomial_roots(R, P), polynomial_eval_eq(RES, P), !.
deb_pretty_polynomial_roots(I, R, RES):-
	pretty_polynomial_roots(R, P),
	write(I), write(' Using roots '), write(R), write(' -> '), output_text(P, RES).

deb_poly_roots(_, P, RES):-
	polynomial_evaluation(P, E), integer_roots_polynomial(E, R),
	msort(R, SR), msort(RES, SRES), SR == SRES, !.
deb_poly_roots(I, P, RES):-
	polynomial_evaluation(P, E), integer_roots_polynomial(E, R),
	write(I), write(' '), write(P), write(' -> '), output_text(R, RES).

deb_poly_roots_eval_roots(_, R):-
	pretty_polynomial_roots(R, P), polynomial_evaluation(P, Q),
	integer_roots_polynomial(Q, RES), sort(R, RS), sort(RES, RESS),
	RS == RESS, !.
deb_poly_roots_eval_roots(I, R):-
	write(I), write(' '),
	pretty_polynomial_roots(R, P),
	polynomial_evaluation(P, Q),
	integer_roots_polynomial(Q, L),
	all_to_string([R, space, ->, space, P, space, ->, space, Q, space, ->, space, L], S1),
	all_to_string([R, space, ->, space, P, space, ->, space, Q, space, ->, space, R], S2),
	output_text(S1, S2), nl.

deb_exp_poly_eval(_, P, V, VAL, RES):-
	expanded_polynomial_evaluation(VAL, V, P, RES), !.
deb_exp_poly_eval(I, P, V, VAL, RES):-
	expanded_polynomial_evaluation(VAL, V, P, R),
	write(I), write(' P('), write(V), write(')='), write(P), write(' -> P('), write(VAL),
	write(')= '), output_text(R, RES).

deb_falling_factorial(_, P, F, RES):-
	polynomial_evaluation(choose(P, F), R), polynomial_evaluation(RES, R), !.
deb_falling_factorial(I, P, F, RES):-
	polynomial_evaluation(choose(P, F), R),
	write(I), write(' choose('), write(P), write(','), write(F), write(')= '), write(R),
	polynomial_evaluation(RES, CORRECT_OUTPUT), output_text(R, CORRECT_OUTPUT).

debug_polynomials:-
	write('-- POLYNOMIAL EVALUATION DEBUG --'), nl,
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

	write(' -> OK'), nl,
	write('* POLYNOMIAL LIST SUM'),

	deb_poly_list_sum(' 1)', [], [], []),
	deb_poly_list_sum(' 2)', [], [0], []),
	deb_poly_list_sum(' 3)', [0], [], []),
	deb_poly_list_sum(' 4)', [0], [0], []),
	deb_poly_list_sum(' 5)', [1], [0], [1]),
	deb_poly_list_sum(' 6)', [0], [1], [1]),
	deb_poly_list_sum(' 7)', [1], [1], [2]),
	deb_poly_list_sum(' 8)', [x], [0], [x]),
	deb_poly_list_sum(' 9)', [x], [x], [2*x]),
	deb_poly_list_sum(' 10)', [x], [-x], []),
	deb_poly_list_sum(' 11)', [2*x], [-x], [x]),
	deb_poly_list_sum(' 12)', [-2*x], [-x], [-3*x]),
	deb_poly_list_sum(' 13)', [3*x], [-4*x], [-x]),
	deb_poly_list_sum(' 14)', [x^2], [-x], [x^2, -x]),
	deb_poly_list_sum(' 15)', [4*x^3], [-x], [4*x^3, -x]),
	deb_poly_list_sum(' 16)', [x, x, x, x], [-x], [3*x]),
	deb_poly_list_sum(' 17)', [x, x, x, x], [-x, x, -x], [3*x]),
	deb_poly_list_sum(' 18)', [2*x, x, x, x], [-x, x, -x], [4*x]),
	deb_poly_list_sum(' 19)', [x, x, 2*x, x], [-x, x, -x], [4*x]),
	deb_poly_list_sum(' 20)', [x, x, x, x^2], [-x, x, -x], [x^2, 2*x]),
	deb_poly_list_sum(' 21)', [x, -x, y, 2*y], [-x, x, -y, -2*y], []),

	write(' -> OK'), nl,
	write('* POLYNOMIAL SORTED LIST SUM'),

	deb_poly_sorted_list_sum(' 1)', [], [], []),
	deb_poly_sorted_list_sum(' 2)', [0], [0], []),
	deb_poly_sorted_list_sum(' 5)', [1], [0], [1]),
	deb_poly_sorted_list_sum(' 6)', [0], [1], [1]),
	deb_poly_sorted_list_sum(' 7)', [1], [1], [2]),
	deb_poly_sorted_list_sum(' 8)', [x], [0], [x]),
	deb_poly_sorted_list_sum(' 9)', [x], [x], [2*x]),
	deb_poly_sorted_list_sum(' 10)', [x], [-x], []),
	deb_poly_sorted_list_sum(' 11)', [2*x], [-x], [x]),
	deb_poly_sorted_list_sum(' 12)', [-2*x], [-x], [-3*x]),
	deb_poly_sorted_list_sum(' 13)', [3*x], [-4*x], [-x]),
	deb_poly_sorted_list_sum(' 14)', [x^2], [-x], [x^2, -x]),
	deb_poly_sorted_list_sum(' 15)', [4*x^3], [-x], [4*x^3, -x]),
	deb_poly_sorted_list_sum(' 16)', [x^3, x^2, x, 1], [-x], [x^3, x^2, 1]),
	deb_poly_sorted_list_sum(' 17)', [x^3, x^2, x, -1], [x], [x^3, x^2, 2*x, -1]),

	write(' -> OK'), nl,
	write('* POLYNOMIAL LIST SUB'),

	deb_poly_list_sub(' 1)', [], [], []),
	deb_poly_list_sub(' 2)', [], [0], []),
	deb_poly_list_sub(' 3)', [0], [], []),
	deb_poly_list_sub(' 4)', [0], [0], []),
	deb_poly_list_sub(' 5)', [1], [0], [1]),
	deb_poly_list_sub(' 6)', [0], [1], [-1]),
	deb_poly_list_sub(' 7)', [1], [1], []),
	deb_poly_list_sub(' 8)', [x], [0], [x]),
	deb_poly_list_sub(' 9)', [x], [x], []),
	deb_poly_list_sub(' 10)', [x], [-x], [2*x]),
	deb_poly_list_sub(' 11)', [2*x], [-x], [3*x]),
	deb_poly_list_sub(' 12)', [-2*x], [-x], [-x]),
	deb_poly_list_sub(' 13)', [3*x], [-4*x], [7*x]),
	deb_poly_list_sub(' 14)', [x^2], [-x], [x^2, x]),
	deb_poly_list_sub(' 15)', [(4/3)*x^3], [-x], [(4/3)*x^3, x]),
	deb_poly_list_sub(' 16)', [x, x, x, x], [-x], [5*x]),
	deb_poly_list_sub(' 17)', [x, x, x, x], [-x, x, -x], [5*x]),
	deb_poly_list_sub(' 18)', [2*x, x, x, x], [-x, x, -x], [6*x]),
	deb_poly_list_sub(' 19)', [x, x, 2*x, x], [-x, x, -x], [6*x]),
	deb_poly_list_sub(' 20)', [x, x, x, x^2], [-x, x, -x], [x^2, 4*x]),

	write(' -> OK'), nl,
	write('* POLYNOMIAL SORTED LIST SUB'),

	deb_poly_sorted_list_sub(' 1)', [], [], []),
	deb_poly_sorted_list_sub(' 2)', [0], [0], []),
	deb_poly_sorted_list_sub(' 5)', [1], [0], [1]),
	deb_poly_sorted_list_sub(' 6)', [0], [1], [-1]),
	deb_poly_sorted_list_sub(' 7)', [1], [1], []),
	deb_poly_sorted_list_sub(' 8)', [x], [0], [x]),
	deb_poly_sorted_list_sub(' 9)', [x], [x], []),
	deb_poly_sorted_list_sub(' 10)', [x], [-x], [2*x]),
	deb_poly_sorted_list_sub(' 11)', [2*x], [-x], [3*x]),
	deb_poly_sorted_list_sub(' 12)', [-2*x], [-x], [-x]),
	deb_poly_sorted_list_sub(' 13)', [3*x], [-4*x], [7*x]),
	deb_poly_sorted_list_sub(' 14)', [x^2], [-x], [x^2, x]),
	deb_poly_sorted_list_sub(' 15)', [4*x^3], [-x], [4*x^3, x]),
	deb_poly_sorted_list_sub(' 16)', [x^3, x^2, x, 1], [-x], [x^3, x^2, 2*x, 1]),
	deb_poly_sorted_list_sub(' 17)', [x^3, x^2, x, -1], [x], [x^3, x^2, -1]),
	deb_poly_sorted_list_sub(' 18)', [x^3, x^2, x, -1], [(1/2)*x^3, x], [(1/2)*x^3, x^2, -1]),

	write(' -> OK'), nl,
	write('* POLYNOMIAL LIST PROD'),

	deb_poly_list_prod(' 1)', [], [], []),
	deb_poly_list_prod(' 2)', [], [0], []),
	deb_poly_list_prod(' 3)', [0], [], []),
	deb_poly_list_prod(' 4)', [0], [0], []),
	deb_poly_list_prod(' 5)', [1], [0], []),
	deb_poly_list_prod(' 6)', [0], [1], []),
	deb_poly_list_prod(' 7)', [1], [1], [1]),
	deb_poly_list_prod(' 8)', [x], [0], []),
	deb_poly_list_prod(' 9)', [x], [x], [x^2]),
	deb_poly_list_prod(' 10)', [x], [-x], [-x^2]),
	deb_poly_list_prod(' 11)', [2*x], [-x], [-2*x^2]),
	deb_poly_list_prod(' 12)', [-2*x], [-x], [2*x^2]),
	deb_poly_list_prod(' 13)', [3*x], [-4*x], [-12*x^2]),
	deb_poly_list_prod(' 14)', [x^2], [-x], [-x^3]),
	deb_poly_list_prod(' 15)', [4*x^3], [-x], [-4*x^4]),
	deb_poly_list_prod(' 16)', [x, x, x, x], [-x], [-4*x^2]),
	deb_poly_list_prod(' 17)', [x, x, x, x], [-x, x, -x], [-4*x^2]),
	deb_poly_list_prod(' 18)', [2*x, x, x, x], [-x, x, -x], [-5*x^2]),
	deb_poly_list_prod(' 19)', [x, x, 2*x, x], [-x, x, -x], [-5*x^2]),
	deb_poly_list_prod(' 20)', [x, x, x, x^2], [-x, x, -x], [-3*x^2, -x^3]),

	write(' -> OK'), nl,
	write('* POLYNOMIAL SORTED LIST PROD'),

	deb_poly_sorted_list_prod(' 1)', [], []),
	deb_poly_sorted_list_prod(' 2)', [x], []),
	deb_poly_sorted_list_prod(' 3)', [x], [x]),
	deb_poly_sorted_list_prod(' 4)', [x^2], [x]),
	deb_poly_sorted_list_prod(' 5)', [x], [x, 1]),
	deb_poly_sorted_list_prod(' 6)', [x^3, x], [x^2, 1]),
	deb_poly_sorted_list_prod(' 7)', [x^3, x^2, x, 1], [x, -3]),
	deb_poly_sorted_list_prod(' 8)', [4*x^4, x^2, -34], [x^3, -2*x^2]),
	deb_poly_sorted_list_prod(' 9)', [4*x^3, x^2, -34], [x^3, -2*x^2]),
	deb_poly_sorted_list_prod(' 10)', [x^20, -x^15, 6*x^12, -24*x^8, 4*x^3, x^2, -34], [x^10, x^5, x^3, -2*x^2]),

	write(' -> OK'), nl,
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

	write(' -> OK'), nl,
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
	deb_poly_prod(' 12)', 0, x, 0),

	write(' -> OK'), nl,
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
	deb_poly_pow(' 16)', 2 + 2, 2, 16),
	deb_poly_pow(' 17)', 0, 2, 0),

	write(' -> OK'), nl,
	write('* POLYNOMIAL EVALUATION'),

	deb_poly_eval(' 1)', x + x - 2, 2*x - 2),
	deb_poly_eval(' 2)', x + x^2 - 2, x^2 + x - 2),
	deb_poly_eval(' 3)', x*x^2 - 2, x^3 - 2),
	deb_poly_eval(' 4)', (x + 1)^2 - 1, x^2 + 2*x),
	deb_poly_eval(' 5)', x*(x + 1)^2 - 1, x^3 + 2*x^2 + x - 1),
	deb_poly_eval(' 6)', x*(x + 1)^2 - x, x^3 + 2*x^2),
	deb_poly_eval(' 7)', (1/8)*x*((x + 1)^2)*(2*x + 1) - (1/16)*x*(x + 1)*(2*x + 1) - (1/16)*x*(x + 1), 1/4*x^4 + 1/2*x^3 + 1/4*x^2),
	deb_poly_eval(' 8)', (-1/8)*(n^2 + n)^2 + (1/12)*n*(n - 2)*(n + 1)*(2*n + 1) + (1/4)*n*(n - 1)*(n + 1), 1/24*n^4 - 1/12*n^3 - 13/24*n^2 - 5/12*n),
	deb_poly_eval(' 9)', (3*x)^5, 243*x^5),
	deb_poly_eval(' 10)', 3*x^5, 3*x^5),

	write(' -> OK'), nl,
	write('* POLYNOMIAL\' FIRST AND LAST MONOMIALS'),

	deb_poly_first_mon(' 1.1)', 3*x - 2*x^2, 3*x, -2*x^2),
	deb_poly_first_mon(' 1.2)', 3*x + 2*x^2, 3*x, 2*x^2),
	deb_poly_first_mon(' 1.3)', 3*x - 2*x^3 + 4*x^4, 3*x, -2*x^3 + 4*x^4),
	deb_poly_first_mon(' 1.4)', 3*x + 2*x^2 + 4*x^3, 3*x, 2*x^2 + 4*x^3),

	deb_poly_last_mon(' 2.1)', 3*x - 2*x^2, 3*x, -2*x^2),
	deb_poly_last_mon(' 2.2)', 3*x + 2*x^2, 3*x, 2*x^2),
	deb_poly_last_mon(' 2.3)', 3*x - 2*x^3 + 4*x^4, 3*x - 2*x^3, 4*x^4),
	deb_poly_last_mon(' 2.4)', 3*x + 2*x^2 + 4*x^3, 3*x + 2*x^2, 4*x^3),

	write(' -> OK'), nl,
	write('* PRETTY POLYNOMIAL ROOTS'),

	deb_pretty_polynomial_roots(' 1)', [2], (x - 2)),
	deb_pretty_polynomial_roots(' 2)', [2,3], (x - 2)*(x - 3)),
	deb_pretty_polynomial_roots(' 3)', [2,3,4], (x - 2)*(x - 3)*(x - 4)),
	deb_pretty_polynomial_roots(' 4)', [2,3,4,5], (x - 2)*(x - 3)*(x - 4)*(x - 5)),
	deb_pretty_polynomial_roots(' 5)', [2,2], (x - 2)^2),
	deb_pretty_polynomial_roots(' 6)', [2,2,3], ((x - 2)^2)*(x - 3)),
	deb_pretty_polynomial_roots(' 7)', [2,3,2], ((x - 2)^2)*(x - 3)),
	deb_pretty_polynomial_roots(' 8)', [2,3,2], ((x - 2)^2)*(x - 3)),
	deb_pretty_polynomial_roots(' 9)', [3,2,2], ((x - 2)^2)*(x - 3)),

	write(' -> OK'), nl,
	write('* POLYNOMIAL ROOTS'),

	deb_poly_roots(' 1)', (x + 1)^2, [-1, -1]),
	deb_poly_roots(' 2)', ((x + 1)^2)*(x - 3), [-1, -1, 3]),
	deb_poly_roots(' 3)', ((x + 1)^2)*(x - 3)^2, [-1, -1, 3, 3]),
	deb_poly_roots(' 4)', ((x + 1)^2)*(x - 3)*(x + 3), [-1, -1, -3, 3]),
	deb_poly_roots(' 5)', (x + 1)*(x - 1)*(x - 3)*(x + 3), [-1, 1, -3, 3]),

	write(' -> OK'), nl,
	write('* POLYNOMIAL ROOTS -> PRETTY -> EVAL -> ROOTS'),

	deb_poly_roots_eval_roots(' 1)', [1]),
	deb_poly_roots_eval_roots(' 2)', [1,1]),
	deb_poly_roots_eval_roots(' 3)', [2,1]),
	deb_poly_roots_eval_roots(' 4)', [2,1,3]),
	deb_poly_roots_eval_roots(' 5)', [2,-1,3]),
	deb_poly_roots_eval_roots(' 6)', [2,-1,-3]),
	deb_poly_roots_eval_roots(' 7)', [-2,1]),
	deb_poly_roots_eval_roots(' 8)', [-2,1,4]),

	write(' -> OK'), nl,
	write('* EXPANDED POLYNOMIAL EVALUATION'),

	deb_exp_poly_eval(' 1)',           3*x, x, 1, 3),
	deb_exp_poly_eval(' 2)',             x, x, 1, 1),
	deb_exp_poly_eval(' 3)',           x^2, x, 1, 1),
	deb_exp_poly_eval(' 4)',       x^2 + x, x, 1, 2),
	deb_exp_poly_eval(' 5)',       x^2 + 3, x, 1, 4),
	deb_exp_poly_eval(' 6)',     4*x^2 + 3, x, -8, 259),
	deb_exp_poly_eval(' 7)',     4*x^2 + 3, x, 7/8, 97/16),
	deb_exp_poly_eval(' 8)',  -4*x^3 + 3*x, x, 0, 0),
	deb_exp_poly_eval(' 9)',  -4*x^3 + 3*x, x, 9/5, -2241/125),
	deb_exp_poly_eval(' 10)', -4*x^3 - 3*x, x, -9/5, 3591/125),
	deb_exp_poly_eval(' 11)',          3*i, i, 1, 3),
	deb_exp_poly_eval(' 12)',            i, i, 1, 1),
	deb_exp_poly_eval(' 13)',          i^2, i, 1, 1),
	deb_exp_poly_eval(' 14)',      i^2 + x, i, 1, x + 1),
	deb_exp_poly_eval(' 15)',      i^2 + 3, i, 1, 4),
	deb_exp_poly_eval(' 16)',    4*i^2 + 3, i, -8, 259),
	deb_exp_poly_eval(' 17)',    4*i^2 + 3, i, 7/8, 97/16),
	deb_exp_poly_eval(' 18)', -4*i^3 + 3*x, i, 0, 3*x),
	deb_exp_poly_eval(' 19)', -4*i^3 + 3*x, i,  9/5, 3*x - 2916/125),
	deb_exp_poly_eval(' 20)', -4*i^3 - 3*x, i, -9/5, -3*x + 2916/125),
	deb_exp_poly_eval(' 21)', 4*x - 4*y + 4*i - 4*x, x, 2, 4*i - 4*y),

	write(' -> OK'), nl,
	write('* FALLING FACTORIALS'),

	deb_falling_factorial(' 1)',    n,		1,	n),
	deb_falling_factorial(' 2)',    n - 1,	1,	n - 1),
	deb_falling_factorial(' 3)',    n - 2,	1,	n - 2),
	deb_falling_factorial(' 4)',    n,		2,	(1/2)*n*(n - 1)),
	deb_falling_factorial(' 5)',    n - 1,	2,	(1/2)*(n - 1)*(n - 2)),
	deb_falling_factorial(' 6)',    n - 2,	2,	(1/2)*(n - 2)*(n - 3)),
	deb_falling_factorial(' 7)',    n,		3,	(1/6)*n*(n - 1)*(n - 2)),
	deb_falling_factorial(' 8)',    n - 1,	3,	(1/6)*(n - 1)*(n - 2)*(n - 3)),
	deb_falling_factorial(' 9)',    n - 2,	3,	(1/6)*(n - 2)*(n - 3)*(n - 4)),
	deb_falling_factorial('10)',    n - 7,	1,	n - 7),
	deb_falling_factorial('11)',    n - 7,	2,	(1/2)*(n - 7)*(n - 8)),
	deb_falling_factorial('12)',    n - 7,	3,	(1/6)*(n - 7)*(n - 8)*(n - 9)),
	deb_falling_factorial('13)',    n - 7,	4,	(1/24)*(n - 7)*(n - 8)*(n - 9)*(n - 10)),
	deb_falling_factorial('14)',  2*n - 7,	4,	(1/24)*(2*n - 7)*(2*n - 8)*(2*n - 9)*(2*n - 10)),
	deb_falling_factorial('15)',  3*n - 7,	4,	(1/24)*(3*n - 7)*(3*n - 8)*(3*n - 9)*(3*n - 10)),
	deb_falling_factorial('16)', -4*n + 7,	4,	(1/24)*(-4*n + 7)*(-4*n + 6)*(-4*n + 5)*(-4*n + 4)),

	write(' -> OK'), nl,
	nl, true.

/*
-----------------------------
DEBUG - POWER SUMS
*/

deb_power_sum_D(_, N, F, D):-
	expanded_polynomial_evaluation(N, n, F, RES), sum_from_1_to_n_to_D(N, D, RES), !.
deb_power_sum_D(I, N, F, D):-
	expanded_polynomial_evaluation(N, n, F, RES), sum_from_1_to_n_to_D(N, D, R),
	write(I), write(' Sum from 1 to '), write(N), write(' = '), output_text(R, RES).

debug_power_sums:-
	F = power_sums,
	write('-- POWER SUMS --'), nl,
	write('* D = 1'),
	call(F, 1, D1),
	deb_power_sum_D(' 1.1)', 1, D1, 1),
	deb_power_sum_D(' 1.2)', 5, D1, 1),
	deb_power_sum_D(' 1.3)', 10, D1, 1),
	deb_power_sum_D(' 1.4)', 20, D1, 1),
	deb_power_sum_D(' 1.5)', 30, D1, 1),
	deb_power_sum_D(' 1.6)', 50, D1, 1),
	deb_power_sum_D(' 1.7)', 75, D1, 1),
	deb_power_sum_D(' 1.8)', 100, D1, 1),

	write(' -> OK'), nl,
	write('* D = 2'),

	call(F, 2, D2),
	deb_power_sum_D(' 2.1)', 1, D2, 2),
	deb_power_sum_D(' 2.2)', 5, D2, 2),
	deb_power_sum_D(' 2.3)', 10, D2, 2),
	deb_power_sum_D(' 2.4)', 20, D2, 2),
	deb_power_sum_D(' 2.5)', 30, D2, 2),
	deb_power_sum_D(' 2.6)', 50, D2, 2),
	deb_power_sum_D(' 2.7)', 75, D2, 2),
	deb_power_sum_D(' 2.8)', 100, D2, 2),

	write(' -> OK'), nl,
	write('* D = 3'),

	call(F, 3, D3),
	deb_power_sum_D(' 3.1)', 1, D3, 3),
	deb_power_sum_D(' 3.2)', 5, D3, 3),
	deb_power_sum_D(' 3.3)', 10, D3, 3),
	deb_power_sum_D(' 3.4)', 20, D3, 3),
	deb_power_sum_D(' 3.5)', 30, D3, 3),
	deb_power_sum_D(' 3.6)', 50, D3, 3),
	deb_power_sum_D(' 3.7)', 75, D3, 3),
	deb_power_sum_D(' 3.8)', 100, D3, 3),

	write(' -> OK'), nl,
	write('* D = 5'),

	call(F, 5, D5),
	deb_power_sum_D(' 5.1)', 1, D5, 5),
	deb_power_sum_D(' 5.2)', 5, D5, 5),
	deb_power_sum_D(' 5.3)', 10, D5, 5),
	deb_power_sum_D(' 5.4)', 20, D5, 5),
	deb_power_sum_D(' 5.5)', 30, D5, 5),
	deb_power_sum_D(' 5.6)', 50, D5, 5),
	deb_power_sum_D(' 5.7)', 75, D5, 5),
	deb_power_sum_D(' 5.8)', 100, D5, 5),

	write(' -> OK'), nl,
	write('* D = 7'),

	call(F, 7, D7),
	deb_power_sum_D(' 7.1)', 1, D7, 7),
	deb_power_sum_D(' 7.2)', 5, D7, 7),
	deb_power_sum_D(' 7.3)', 10, D7, 7),
	deb_power_sum_D(' 7.4)', 20, D7, 7),
	deb_power_sum_D(' 7.5)', 30, D7, 7),
	deb_power_sum_D(' 7.6)', 50, D7, 7),
	deb_power_sum_D(' 7.7)', 75, D7, 7),
	deb_power_sum_D(' 7.8)', 100, D7, 7),

	write(' -> OK'), nl,
	write('* D = 10'),

	call(F, 10, D10),
	deb_power_sum_D(' 10.1)', 1, D10, 10),
	deb_power_sum_D(' 10.2)', 5, D10, 10),
	deb_power_sum_D(' 10.3)', 10, D10, 10),
	deb_power_sum_D(' 10.4)', 20, D10, 10),
	deb_power_sum_D(' 10.5)', 30, D10, 10),
	deb_power_sum_D(' 10.6)', 50, D10, 10),
	deb_power_sum_D(' 10.7)', 75, D10, 10),
	deb_power_sum_D(' 10.8)', 100, D10, 10),

	write(' -> OK'), nl,
	write('* D = 15'),

	call(F, 15, D15),
	deb_power_sum_D(' 15.1)', 1, D15, 15),
	deb_power_sum_D(' 15.2)', 5, D15, 15),
	deb_power_sum_D(' 15.3)', 10, D15, 15),
	deb_power_sum_D(' 15.4)', 20, D15, 15),
	deb_power_sum_D(' 15.5)', 30, D15, 15),
	deb_power_sum_D(' 15.6)', 50, D15, 15),
	deb_power_sum_D(' 15.7)', 75, D15, 15),
	deb_power_sum_D(' 15.8)', 100, D15, 15),

	write(' -> OK'), nl,
	write('* D = 20'),

	call(F, 20, D20),
	deb_power_sum_D(' 20.1)', 1, D20, 20),
	deb_power_sum_D(' 20.2)', 5, D20, 20),
	deb_power_sum_D(' 20.3)', 10, D20, 20),
	deb_power_sum_D(' 20.4)', 20, D20, 20),
	deb_power_sum_D(' 20.5)', 30, D20, 20),
	deb_power_sum_D(' 20.6)', 50, D20, 20),
	deb_power_sum_D(' 20.7)', 75, D20, 20),
	deb_power_sum_D(' 20.8)', 100, D20, 20),

	write(' -> OK'), nl,
	nl, true.
