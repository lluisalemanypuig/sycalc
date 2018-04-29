:-ensure_loaded(core).
:-ensure_loaded(algorithms).

debug:-
	%debug_integer_algs,
	%debug_lists,
	%debug_numbers,
	%debug_arithmetic_evaluation,
	%debug_monomials,
	%debug_polynomials,
	debug_power_sums,
	true.

main:- debug, halt.
main:- nl, write('ERROR'), nl, halt.

output_text(OBJ, EXP):- write(OBJ), write(' | Not correct - Expected to see: '), write(EXP), nl, false.
output_correct(I):- write('  '), write(I), write(' Ok'), nl.

all_to_string([space], ' '):- !.
all_to_string([X], S):- term_to_atom(X, S), !.
all_to_string([space|L], S):- all_to_string(L, LS), atom_concat(' ', LS, S), !.
all_to_string([X|L], S):- term_to_atom(X, AX), all_to_string(L, LS), atom_concat(AX, LS, S), !.

/*
-----------------------------
DEBUG - INTEGER ALGORITHMS
*/

deb_divisors(I, N, RES):- divisors(N, R), R == RES, !, output_correct(I).
deb_divisors(I, N, RES):-
	divisors(N, R),
	write(I), write(' Divisors( '), write(N), write(' ): '), output_text(R, RES).

deb_gcd(I, A, B, RES):- gcd(A, B, R), R == RES, !, output_correct(I).
deb_gcd(I, A, B, RES):-
	gcd(A, B, R),
	write(I), write(' gcd('), write(A), write(','), write(B), write(')= '), output_text(R, RES).

debug_integer_algs:-
	write('-- INTEGER ALGORITHMS DEBUG --'), nl,
	write('* DIVISORS'), nl,

	deb_divisors('  1)', 1, [1,-1]),
	deb_divisors('  2)', 2, [1,-1,2,-2]),
	deb_divisors('  3)', 3, [1,-1,3,-3]),
	deb_divisors('  4)', 6, [1,-1,2,-2,3,-3,6,-6]),
	deb_divisors('  5)', 8, [1,-1,2,-2,4,-4,8,-8]),
	deb_divisors('  6)', 12, [1,-1,2,-2,3,-3,4,-4,6,-6,12,-12]),
	deb_divisors('  7)', 20, [1,-1,2,-2,4,-4,5,-5,10,-10,20,-20]),
	deb_divisors('  8)', 24, [1,-1,2,-2,3,-3,4,-4,6,-6,8,-8,12,-12,24,-24]),
	deb_divisors('  9)', 32, [1,-1,2,-2,4,-4,8,-8,16,-16,32,-32]),
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

	write('* GREATEST COMMON DIVISOR'), nl,

	deb_gcd('  1)', 1, 1, 1),
	deb_gcd('  2)', 2, 1, 1),
	deb_gcd('  3)', 1, 2, 1),
	deb_gcd('  4)', 3, 3, 3),
	deb_gcd('  5)', 7, 3, 1),
	deb_gcd('  6)', 3, 7, 1),
	deb_gcd('  7)', 8, 4, 4),
	deb_gcd('  8)', 4, 8, 4),
	deb_gcd('  9)', 12, 8, 4),
	deb_gcd(' 10)', 8, 12, 4),
	deb_gcd(' 11)', 8, 16, 8),
	deb_gcd(' 12)', 120, 49, 1),
	deb_gcd(' 13)', 5040, 49, 7),
	deb_gcd(' 14)', 42, 49, 7),
	deb_gcd(' 15)', 247, 270, 1),
	deb_gcd(' 16)', 247, -270, 1),
	deb_gcd(' 17)', -247, 270, 1),
	deb_gcd(' 18)', -247, -270, 1),

	nl, true.

/*
-----------------------------
DEBUG - LIST OPERATIONS
*/

deb_insertion_sort(I, L):-
	isort(L, R), msort(L, R), !, output_correct(I).
deb_insertion_sort(I, L):-
	isort(L, R),
	sort(L, RES), write(I), write(L), write(' -> '), output_text(R, RES).

deb_pinsertion_sort(I, L1,L2, E1,E2):- pisort(L1,L2, E1,E2), !, output_correct(I).
deb_pinsertion_sort(I, L1,L2, E1,E2):-
	pisort(L1,L2, R1,R2),
	write(I), write(' Pair-wise sorting not correct'), nl,
	write('    Sorted '), write(L1), write(' is '), write(R1), write(' but expected '), write(E1), nl,
	write('    Sorted '), write(L2), write(' is '), write(R2), write(' but expected '), write(E2), nl.

deb_how_many(I, L, RES):- how_many(L, RES), !, output_correct(I).
deb_how_many(I, L, RES):-
	how_many(L, R),
	write(I), write(' Count of '), write(L), write(': '), output_text(R, RES).

deb_cart_prod(I, A, B, RES):- cartesian_product(A, B, RES), !, output_correct(I).
deb_cart_prod(I, A, B, RES):-
	cartesian_product(A, B, R),
	write(I), write(' '), write(A), write(' x '), write(B), write(' = '), output_text(R, RES).

deb_cart_prod_by(I, A, B, RES):- cartesian_product_by(plus, A, B, RES), !, output_correct(I).
deb_cart_prod_by(I, A, B, RES):-
	cartesian_product_by(plus, A, B, R),
	write(I), write(' '), write(A), write(' (x) '), write(B), write(' = '), output_text(R, RES).

debug_lists:-
	write('-- LISTS OPERATIONS DEBUG --'), nl,
	write('* SORTING ALGORITHMS - INSERTION SORT'), nl,

	deb_insertion_sort('  1) ', [3,2,1]),
	deb_insertion_sort('  2) ', [123,4,46,7,578,67,8567,58,21,23,4,245,3,2,1]),
	deb_insertion_sort('  3) ', [-4,8567,25,123,4,46,7,-8,578,67,-4,8567,58,21,-7,23,4,245,3,2,1,8567]),

	write('* SORTING ALGORITHMS - PAIRWISE INSERTION SORT'), nl,

	deb_pinsertion_sort('  1)', [x,y,z],[1,2,3], [x,y,z],[1,2,3]),
	deb_pinsertion_sort('  2)', [x,y,z],[3,2,1], [x,y,z],[3,2,1]),
	deb_pinsertion_sort('  3)', [z,y,x],[3,2,1], [x,y,z],[1,2,3]),
	deb_pinsertion_sort('  4)', [y,z,x],[2,1,3], [x,y,z],[3,2,1]),
	deb_pinsertion_sort('  5)', [z,x,y],[3,1,2], [x,y,z],[1,2,3]),

	write('* COUNTING HOW MANY'), nl,

	deb_how_many('  1)', [1], [[1,1]]),
	deb_how_many('  2)', [1,1], [[1,2]]),
	deb_how_many('  3)', [1,1,1], [[1,3]]),
	deb_how_many('  4)', [2,1,1,1], [[1,3],[2,1]]),
	deb_how_many('  5)', [2,1,1,2], [[1,2],[2,2]]),
	deb_how_many('  6)', [3,1,1,2], [[1,2],[2,1],[3,1]]),
	deb_how_many('  7)', [3,1,1,2,7], [[1,2],[2,1],[3,1],[7,1]]),
	deb_how_many('  8)', [3,1,1,2,1], [[1,3],[2,1],[3,1]]),
	deb_how_many('  9)', [3,1,3,2,1], [[1,2],[2,1],[3,2]]),
	deb_how_many(' 10)', [4,1,3,2,1], [[1,2],[2,1],[3,1],[4,1]]),

	write('* CARTESIAN PRODUCT'), nl,

	deb_cart_prod('  1)', [], [], []),
	deb_cart_prod('  2)', [1], [], []),
	deb_cart_prod('  3)', [1,2], [], []),
	deb_cart_prod('  4)', [], [1], []),
	deb_cart_prod('  5)', [], [1,2], []),
	deb_cart_prod('  6)', [1], [1], [[1,1]]),
	deb_cart_prod('  7)', [1,2], [1], [[1,1],[2,1]]),
	deb_cart_prod('  8)', [1], [1,2], [[1,1],[1,2]]),
	deb_cart_prod('  9)', [1,3], [1,2], [[1,1],[1,2],[3,1],[3,2]]),
	deb_cart_prod(' 10)', [1,2], [1,3], [[1,1],[1,3],[2,1],[2,3]]),

	write('* CARTESIAN PRODUCT BY'), nl,

	deb_cart_prod_by('  1)', [], [], []),
	deb_cart_prod_by('  2)', [1], [], []),
	deb_cart_prod_by('  3)', [1,2], [], []),
	deb_cart_prod_by('  4)', [], [1], []),
	deb_cart_prod_by('  5)', [], [1,2], []),
	deb_cart_prod_by('  6)', [1], [1], [2]),
	deb_cart_prod_by('  7)', [1,2], [1], [2,3]),
	deb_cart_prod_by('  8)', [1], [1,2], [2,3]),
	deb_cart_prod_by('  9)', [1,3], [1,2], [2,3,4,5]),
	deb_cart_prod_by(' 10)', [1,2], [1,3], [2,4,3,5]),

	nl, true.

/*
-----------------------------
DEBUG - NUMBERS
*/

deb_red_frac(I, F, RES):- red_frac(F, RES), !, output_correct(I).
deb_red_frac(I, F, RES):- red_frac(F, R), write(I), write(' '), write(F), write(' = '), output_text(R, RES).

deb_rat_gcd(I, F1, F2, RES1, RES2, RES3):- rational_gcd_rel(F1, F2, RES1, RES2, RES3), !, output_correct(I).
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
	write('* REDUCED FRACTIONS'), nl,

	deb_red_frac('  1)', 0/1, 0),
	deb_red_frac('  2)', 0/5, 0),
	deb_red_frac('  3)', 1/2, 1/2),
	deb_red_frac('  4)', 4/2, 2),
	deb_red_frac('  5)', 5/2, 5/2),
	deb_red_frac('  6)', 6/2, 3),
	deb_red_frac('  7)', 6/4, 3/2),
	deb_red_frac('  8)', 6/8, 3/4),
	deb_red_frac('  9)', 6/10, 3/5),
	deb_red_frac(' 10)', -0/1, 0),
	deb_red_frac(' 11)', -0/5, 0),
	deb_red_frac(' 12)', -1/2, -1/2),
	deb_red_frac(' 13)', -4/2, -2),
	deb_red_frac(' 14)', -5/2, -5/2),
	deb_red_frac(' 15)', -6/2, -3),
	deb_red_frac(' 16)', -6/4, -3/2),
	deb_red_frac(' 17)', -6/8, -3/4),
	deb_red_frac(' 18)', -6/10, -3/5),

	write('* RATIONAL GREATEST COMMON DIVISOR'), nl,

	deb_rat_gcd('  1)', 1, 2, 1, 1, 2),
	deb_rat_gcd('  2)', 2, 2, 2, 1, 1),
	deb_rat_gcd('  3)', 4, 2, 2, 2, 1),
	deb_rat_gcd('  4)', 4, 4, 4, 1, 1),
	deb_rat_gcd('  5)', 7, 3, 1, 7, 3),
	deb_rat_gcd('  6)', 18, 12, 6, 3, 2),
	deb_rat_gcd('  7)', 15, 12, 3, 5, 4),
	deb_rat_gcd('  8)', 1/3, 1/6, 1/3, 1/1, 1/2),
	deb_rat_gcd('  9)', -1/3, 1/6, 1/3, -1/1, 1/2),
	deb_rat_gcd(' 10)', 1/3, -1/6, 1/3, 1/1, -1/2),
	deb_rat_gcd(' 11)', -1/3, -1/6, 1/3, -1/1, -1/2),
	deb_rat_gcd(' 12)', 5, 5/3, 5, 1, 1/3),
	deb_rat_gcd(' 13)', 2, 1/2, 1, 2, 1/2),
	deb_rat_gcd(' 14)', 2, -1/2, 1, 2, -1/2),

	nl, true.

/*
-----------------------------
DEBUG - ARITHMETIC EVALUATION
*/

deb_sum(I, A, B, RES):- eval_sum(A, B, RES), !, output_correct(I).
deb_sum(I, A, B, RES):-
	eval_sum(A, B, R),
	write(I), write(' '), write(A), write(' + '), write(B), write(' = '),
	output_text(R, RES).

deb_sub(I, A, B, RES):- eval_sub(A, B, RES), !, output_correct(I).
deb_sub(I, A, B, RES):-
	eval_sub(A, B, R),
	write(I), write(' '), write(A), write(' - '), write(B), write(' = '),
	output_text(R, RES).

deb_prod(I, A, B, RES):- eval_prod(A, B, RES), !, output_correct(I).
deb_prod(I, A, B, RES):-
	eval_sub(A, B, R),
	write(I), write(' '), write(A), write(' * '), write(B), write(' = '),
	output_text(R, RES).

deb_power(I, A, B, RES):- eval_pow(A, B, RES), !, output_correct(I).
deb_power(I, A, B, RES):-
	eval_pow(A, B, R),
	write(I), write(' '), write(A), write('^'), write(B), write(' = '),
	output_text(R, RES).

deb_arithm(I, E, RES):- arithmetic_eval(E, RES), !, output_correct(I).
deb_arithm(I, E, RES):-
	arithmetic_eval(E, R),
	write(I), write(' '), write(E), write(' = '), output_text(R, RES).

deb_abs(I, E, RES):- abs_real(E, RES), !, output_correct(I).
deb_abs(I, E, RES):-
	abs_real(E, R),
	write(I), write(' |'), write(E), write('| = '), output_text(R, RES), !.

debug_arithmetic_evaluation:-
	write('-- ARITHMETIC EVALUATION DEBUG --'), nl,
	write('* SUMS'), nl,

	deb_sum('  1)', 0, 0, 0),
	deb_sum('  2)', 1, 0, 1),
	deb_sum('  3)', 4, 0, 4),
	deb_sum('  4)', 0, 1, 1),
	deb_sum('  5)', 0, 5, 5),
	deb_sum('  6)', 1/2, 1/2, 1),
	deb_sum('  7)', 1/2, 2/2, 3/2),
	deb_sum('  8)', 1/2, 1, 3/2),
	deb_sum('  9)', 1, 1/2, 3/2),
	deb_sum(' 10)', 2/2, 2/2, 2),
	deb_sum(' 11)', 2/2, 3/2, 5/2),
	deb_sum(' 12)', 3/2, 1, 5/2),

	write('* SUBS'), nl,

	deb_sub('  1)', 0, 0, 0),
	deb_sub('  2)', 1, 0, 1),
	deb_sub('  3)', 4, 0, 4),
	deb_sub('  4)', 0, 1, -1),
	deb_sub('  5)', 0, 5, -5),
	deb_sub('  6)', 1/2, 1/2, 0),
	deb_sub('  7)', 1/2, 2/2, -1/2),
	deb_sub('  8)', 1/2, 1, -1/2),
	deb_sub('  9)', 1, 1/2, 1/2),
	deb_sub(' 10)', 2/2, 2/2, 0),
	deb_sub(' 11)', 2/2, 3/2, -1/2),
	deb_sub(' 12)', 3/2, 1, 1/2),
	deb_sub(' 13)', 0, 1/25, -1/25),

	write('* PRODS'), nl,

	deb_prod('  1)', 0, 0, 0),
	deb_prod('  2)', 1, 0, 0),
	deb_prod('  3)', 4, 0, 0),
	deb_prod('  4)', 0, 1, 0),
	deb_prod('  5)', 0, 5, 0),
	deb_prod('  6)', 1/2, 1/2, 1/4),
	deb_prod('  7)', 1/2, 2/2, 1/2),
	deb_prod('  8)', 1/2, 1, 1/2),
	deb_prod('  9)', 1, 1/2, 1/2),
	deb_prod(' 10)', 2/2, 2/2, 1),
	deb_prod(' 11)', 2/2, 3/2, 3/2),
	deb_prod(' 12)', 3/2, 1, 3/2),

	write('* POWERS'), nl,

	deb_power('  1)', 0, 0, 1),
	deb_power('  2)', 1, 0, 1),
	deb_power('  3)', 4, 0, 1),
	deb_power('  4)', 0, 1, 0),
	deb_power('  5)', 0, 5, 0),
	deb_power('  6)', 1/2, 2, 1/4),
	deb_power('  7)', 1/2, 1, 1/2),
	deb_power('  8)', 2, 3, 8),
	deb_power('  9)', 2/2, 2, 1),
	deb_power(' 10)', 2/2, 3, 1),
	deb_power(' 11)', 3/2, 1, 3/2),
	deb_power(' 12)', 3/2, 2, 9/4),

	write('* ARITHMETIC EXPRESSIONS'), nl,

	deb_arithm('  1)', 3 + 3, 6),
	deb_arithm('  2)', 3 - 4, -1),
	deb_arithm('  3)', 3^4, 81),
	deb_arithm('  4)', 2^2^2^2, 256),
	deb_arithm('  5)', 2^2^2^2^2, 65536),
	deb_arithm('  6)', 2^(1 + 1)^2, 16),
	deb_arithm('  7)', 2^2^(2 + 1 - 1)^2^2, 65536),
	deb_arithm('  8)', (1/2)^2 + 3/4, 1),
	deb_arithm('  9)', (1/2)^0, 1),
	deb_arithm(' 10)', (1/2)^0 + 3/4, 7/4),
	deb_arithm(' 11)', 1 - (1 + 1), -1),
	deb_arithm(' 12)', 1/2, 1/2),

	write('* ABSOLUTE VALUES'), nl,

	deb_abs('  1)', 3, 3),
	deb_abs('  2)', -3, 3),
	deb_abs('  3)', 0, 0),
	deb_abs('  4)', -0, 0),
	deb_abs('  5)', 0, 0),
	deb_abs('  6)', 1/2, 1/2),
	deb_abs('  7)', -1/2, 1/2),
	deb_abs('  8)', 0/2, 0),
	deb_abs('  9)', -0/2, 0),
	deb_abs(' 10)', 1.4142, 1.4142),
	deb_abs(' 11)', -1.4142, 1.4142),

	nl, true.

/*
-----------------------------
DEBUG - MONOMIAL EVALUATION
*/

deb_mon_def(I, M, EXP):- monomial(M), EXP == 'YES', !, output_correct(I).
deb_mon_def(I, M, EXP):- not(monomial(M)), EXP == 'NO', !, output_correct(I).
deb_mon_def(I, M, EXP):-
	monomial(M), EXP == 'NO',
	write(I), write(' string '),
	write(M), write(' is a monomial, NOT as expected'), nl, !.
deb_mon_def(I, M, EXP):-
	not(monomial(M)), EXP == 'YES',
	write(I), write(' string '),
	write(M), write(' is not a monomial, but we expected it to be'), nl.

deb_mon_comp(I, M, C,V,E):- monomial_comps(M, C,V,E), !, output_correct(I).
deb_mon_comp(I, M, RC,RV,RE):-
	monomial_comps(M, C, V, E),
	write(I), write(' Components of '), write(M), write(' are ('),
	write(C), write(','), write(V), write(','), write(E), write(') but were expected to be '),
	write('('), write(RC), write(','), write(RV), write(','), write(RE), write(')'), false.

deb_mon_reduced_vars(I, V,E, EV,EE):-
	red_monomial_vars_list(V,E, EV,EE), !, output_correct(I).
deb_mon_reduced_vars(I, V,E, EV,EE):-
	red_monomial_vars_list(V,E, RV,RE),
	write(I), write(' The reduced variables are '), nl,
	write('    V='), write(RV), write(' but expected '), write(EV), nl,
	write('    E='), write(RE), write(' but expected '), write(EE), nl,
	false.

deb_mon_reduced_comps(I, C,V,E, EC,EV,EE):-
	red_monomial_comps(C,V,E, EC,EV,EE), !, output_correct(I).
deb_mon_reduced_comps(I, C,V,E, EC,EV,EE):-
	red_monomial_comps(C,V,E, RC,RV,RE),
	write(I), write(' The reduced components are '), nl,
	write('    C='), write(RC), write(' but expected '), write(EC), nl,
	write('    V='), write(RV), write(' but expected '), write(EV), nl,
	write('    E='), write(RE), write(' but expected '), write(EE), nl,
	false.

deb_mon_obtention_comps(I, C,V,E, M):-
	red_monomial_from_comps(C,V,E, M), !, output_correct(I).
deb_mon_obtention_comps(I, C,V,E, M):-
	red_monomial_from_comps(C,V,E, RM),
	write(I), write(' The reduced monomial is '), output_text(RM, M).

deb_red_mon(I, M, RES):- red_monomial(M, RES), !, output_correct(I).
deb_red_mon(I, M, RES):-
	red_monomial(M, R),
	write(I), write(' '), write(M), write(' = '), output_text(R, RES).

deb_mon_comparison(I, M1,M2):- monomial_comp(M1,M2), !, output_correct(I).
deb_mon_comparison(I, M1,M2):-
	write(I), write(' '),
	write(M1), write(' < '), write(M2), write(' is false'), nl,
	false.

deb_mon_sort(I, L, E):- monomial_sort(L, S), E == S, !, output_correct(I).
deb_mon_sort(I, L, E):-
	monomial_sort(L, S),
	write(I), write(L), write(' sorted is '), output_text(S, E).

deb_mon_sum(I, M1, M2, RES):-
	mon_sum(M1, M2, R), RES == R, !, output_correct(I).
deb_mon_sum(I, M1, M2, RES):-
	mon_sum(M1, M2, S),
	write(I), write(' '), write(M1), write(' + '), write(M2), write(' = '),
	output_text(S, RES).

deb_mon_sub(I, M1, M2, RES):-
	mon_sub(M1, M2, R), RES == R, !, output_correct(I).
deb_mon_sub(I, M1, M2, RES):-
	mon_sub(M1, M2, S),
	write(I), write(' '), write(M1), write(' - '), write(M2), write(' = '),
	output_text(S, RES).

deb_mon_prod(I, M1, M2, RES):-
	mon_prod(M1, M2, R), RES == R, !, output_correct(I).
deb_mon_prod(I, M1, M2, RES):-
	mon_prod(M1, M2, S),
	write(I), write(' ('), write(M1), write(')'), write('*'), write('('),
	write(M2), write(')'), write(' = '), output_text(S, RES).

deb_mon_pos_coef(I, M1, RES):-
	monomial_positive_coefficient(M1), RES == 'YES', !, output_correct(I).
deb_mon_pos_coef(I, M1, RES):-
	not(monomial_positive_coefficient(M1)), RES == 'NO', !, output_correct(I).
deb_mon_pos_coef(I, M1, RES):-
	monomial_positive_coefficient(M1), RES == 'NO', !,
	write(I), write(M1), write(' '), output_text('YES', RES).
deb_mon_pos_coef(I, M1, RES):-
	not(monomial_positive_coefficient(M1)), RES == 'YES',
	write(I), write(' '), write(M1), write(' -> '),
	output_text('NO', RES).

deb_mon_eval(I, M, V, VAL, RES):-
	monomial_evaluation(VAL, V, M, R), RES == R, !, output_correct(I).
deb_mon_eval(I, M, V, VAL, RES):-
	monomial_evaluation(VAL, V, M, R),
	write(I),
	write(' M('), write(V), write('|'), write(VAL), write(')='),
	output_text(R, RES).

deb_mon_revar(I, O,Vi, M, RES):-
	monomial_revar(O,Vi, M, R), RES == R, !, output_correct(I).
deb_mon_revar(I, O,Vi, M, RES):-
	monomial_revar(O,Vi, M, R),
	write(I),
	write(M), write('|{'), write(O), write('->'), write(Vi), write('} = '),
	output_text(R, RES).

deb_mon_split(I, V, M, Mo,Mr):-
	monomial_split(V, M, Vo,Wr), Vo == Mo, Wr == Mr, !, output_correct(I).
deb_mon_split(I, V, M, Mo,Mr):-
	monomial_split(V, M, Vo,Wr),
	write(I), write('Splitting monomial '), write(M), write(' at '), write(V), write(' produces:'), nl,
	write('     Vo='), write(Vo), write(' but expected '), write(Mo), nl,
	write('     Wr='), write(Wr), write(' but expected '), write(Mr), nl,
	false.

debug_monomials:-
	write('-- MONOMIAL EVALUATION DEBUG --'), nl,
	write('* MONOMIAL DEFINITION'), nl,

	deb_mon_def('  1)', 3, 'YES'),
	deb_mon_def('  2)', x, 'YES'),
	deb_mon_def('  3)', 3*x, 'YES'),
	deb_mon_def('  4)', 3*x^0, 'YES'),
	deb_mon_def('  5)', x^0, 'YES'),
	deb_mon_def('  6)', (3+3)*x^(3 + 1/2), 'YES'),
	deb_mon_def('  7)', 3 + 3, 'YES'),
	deb_mon_def('  8)', 3*3, 'YES'),

	write('* COMPONENT EXTRACTION'), nl,

	deb_mon_comp('  1)', 0*x^0,                 0, [x], [0]),
	deb_mon_comp('  2)', 0*x^1,                 0, [x], [1]),
	deb_mon_comp('  3)', 3*x^0,                 3, [x], [0]),
	deb_mon_comp('  4)', 3*x^1,		            3, [x], [1]),
	deb_mon_comp('  5)', 3*x^4,                 3, [x], [4]),
	deb_mon_comp('  6)', (3 + 1)*x^4,           4, [x], [4]),
	deb_mon_comp('  7)', -2*x^4,               -2, [x], [4]),
	deb_mon_comp('  8)', (-3 + 1)*x^4,         -2, [x], [4]),
	deb_mon_comp('  9)', (-3 + 1)*x^(4 - 4),   -2, [x], [0]),
	deb_mon_comp(' 10)', (-3*3 + 1)*x^(4 - 4), -8, [x], [0]),
	deb_mon_comp(' 11)', 3*x*y,                 3, [x,y], [1,1]),
	deb_mon_comp(' 12)', 3*x*y*z,               3, [x,y,z], [1,1,1]),
	deb_mon_comp(' 13)', 3*x^3*y^0*z^(1/2),     3, [x,y,z], [3,0,1/2]),
	deb_mon_comp(' 14)', 3*x*z,                 3, [x,z], [1,1]),
	deb_mon_comp(' 15)', -3*x*y,               -3, [x,y], [1,1]),
	deb_mon_comp(' 16)', -3*x*y*z,             -3, [x,y,z], [1,1,1]),
	deb_mon_comp(' 17)', -3*x^3*y^0*z^(1/2),   -3, [x,y,z], [3,0,1/2]),
	deb_mon_comp(' 18)', -3*x*z,               -3, [x,z], [1,1]),
	deb_mon_comp(' 19)', -1*x^0*y^0*z^0,       -1, [x,y,z], [0,0,0]),
	deb_mon_comp(' 20)', -2*x^0*y^0*z^1,       -2, [x,y,z], [0,0,1]),
	deb_mon_comp(' 21)', -1*x^0*y^0*z,         -1, [x,y,z], [0,0,1]),
	deb_mon_comp(' 22)', -2*x^0*y^0*z^2,       -2, [x,y,z], [0,0,2]),
	deb_mon_comp(' 23)', -2*x^0*y^1*z^0,       -2, [x,y,z], [0,1,0]),
	deb_mon_comp(' 24)', -2*x^0*y*z^0,         -2, [x,y,z], [0,1,0]),
	deb_mon_comp(' 25)', -1*x^0*y^2*z^0,       -1, [x,y,z], [0,2,0]),
	deb_mon_comp(' 26)', -2*x^0*y^2*z^1,       -2, [x,y,z], [0,2,1]),
	deb_mon_comp(' 27)', -2*x^0*y^2*z^2,       -2, [x,y,z], [0,2,2]),
	deb_mon_comp(' 28)', -1*x^1*y^2*z^2,       -1, [x,y,z], [1,2,2]),
	deb_mon_comp(' 29)', -2*x*y^2*z^2,         -2, [x,y,z], [1,2,2]),
	deb_mon_comp(' 30)', -2*x^2*y^2*z^2,       -2, [x,y,z], [2,2,2]),

	write('* VARIABLE REDUCTION'), nl,

	deb_mon_reduced_vars('  1)', [x,y,z],[0,0,0], [],[]),
	deb_mon_reduced_vars('  2)', [x,y,z],[1,0,0], [x],[1]),
	deb_mon_reduced_vars('  3)', [x,y,z],[0,1,0], [y],[1]),
	deb_mon_reduced_vars('  4)', [x,y,z],[0,0,1], [z],[1]),
	deb_mon_reduced_vars('  5)', [x,y,z],[1,1,0], [x,y],[1,1]),
	deb_mon_reduced_vars('  6)', [x,y,z],[1,0,1], [x,z],[1,1]),
	deb_mon_reduced_vars('  7)', [x,y,z],[0,1,1], [y,z],[1,1]),
	deb_mon_reduced_vars('  8)', [x,y,z],[1,1,1], [x,y,z],[1,1,1]),
	deb_mon_reduced_vars('  9)', [x,y,z],[2,1,0], [x,y],[2,1]),
	deb_mon_reduced_vars(' 10)', [x,y,z],[0,1,2], [y,z],[1,2]),
	deb_mon_reduced_vars(' 11)', [x,y,z],[1,0,2], [x,z],[1,2]),

	write('* COMPONENT REDUCTION'), nl,

	deb_mon_reduced_comps('  1)',  0,[x,y,z],[1,1,1],  0,[],[]),
	deb_mon_reduced_comps('  1)',  1,[x,y,z],[0,0,0],  1,[],[]),
	deb_mon_reduced_comps('  2)',  1,[x,y,z],[1,0,0],  1,[x],[1]),
	deb_mon_reduced_comps('  3)',  1,[x,y,z],[0,1,0],  1,[y],[1]),
	deb_mon_reduced_comps('  4)',  1,[x,y,z],[0,0,1],  1,[z],[1]),
	deb_mon_reduced_comps('  5)',  1,[x,y,z],[1,1,0],  1,[x,y],[1,1]),
	deb_mon_reduced_comps('  6)', -1,[x,y,z],[1,0,1], -1,[x,z],[1,1]),
	deb_mon_reduced_comps('  7)', -1,[x,y,z],[0,1,1], -1,[y,z],[1,1]),
	deb_mon_reduced_comps('  8)', -1,[x,y,z],[1,1,1], -1,[x,y,z],[1,1,1]),
	deb_mon_reduced_comps('  9)', -1,[x,y,z],[2,1,0], -1,[x,y],[2,1]),
	deb_mon_reduced_comps(' 10)', -1,[x,y,z],[0,1,2], -1,[y,z],[1,2]),
	deb_mon_reduced_comps(' 11)', -1,[x,y,z],[1,0,2], -1,[x,z],[1,2]),
	deb_mon_reduced_comps(' 12)',  2,[x,y,z],[1,0,1],  2,[x,z],[1,1]),
	deb_mon_reduced_comps(' 13)',  2,[x,y,z],[0,1,1],  2,[y,z],[1,1]),
	deb_mon_reduced_comps(' 14)',  2,[x,y,z],[1,1,1],  2,[x,y,z],[1,1,1]),
	deb_mon_reduced_comps(' 15)',  2,[x,y,z],[2,1,0],  2,[x,y],[2,1]),
	deb_mon_reduced_comps(' 16)',  2,[x,y,z],[0,1,2],  2,[y,z],[1,2]),
	deb_mon_reduced_comps(' 17)',  2,[x,y,z],[1,0,2],  2,[x,z],[1,2]),

	write('* MONOMIAL OBTENTION FROM COMPONENTS'), nl,

	deb_mon_obtention_comps('  1)',  1,[x,y,z],[0,0,0], 1),
	deb_mon_obtention_comps('  2)',  1,[x,y,z],[1,0,0], x),
	deb_mon_obtention_comps('  2)', -1,[x,y,z],[1,0,0], -x),
	deb_mon_obtention_comps('  3)',  2,[x,y,z],[0,1,0], 2*y),
	deb_mon_obtention_comps('  4)', -2,[x,y,z],[0,0,1], -2*z),
	deb_mon_obtention_comps('  5)',  2,[x,y,z],[1,0,1], 2*x*z),
	deb_mon_obtention_comps('  6)',  2,[x,y,z],[1,1,0], 2*x*y),
	deb_mon_obtention_comps('  7)',  2,[x,y,z],[0,1,1], 2*y*z),
	deb_mon_obtention_comps('  8)',  2,[x,y,z],[1,1,1], 2*x*y*z),
	deb_mon_obtention_comps('  9)',  2,[x,y,z],[2,1,1], 2*x^2*y*z),
	deb_mon_obtention_comps(' 10)',  2,[x,y,z],[1,2,1], 2*x*y^2*z),
	deb_mon_obtention_comps(' 11)',  2,[x,y,z],[1,1,2], 2*x*y*z^2),
	deb_mon_obtention_comps(' 12)',  2,[x,y,z],[2,1,2], 2*x^2*y*z^2),
	deb_mon_obtention_comps(' 13)',  2,[x,y,z],[2,2,1], 2*x^2*y^2*z),
	deb_mon_obtention_comps(' 14)',  2,[x,y,z],[1,2,2], 2*x*y^2*z^2),
	deb_mon_obtention_comps(' 15)',  2,[x,y,z],[2,2,2], 2*x^2*y^2*z^2),
	deb_mon_obtention_comps(' 16)',  1,[z,x,y],[1,0,0], z),
	deb_mon_obtention_comps(' 17)', -1,[z,x,y],[1,0,0], -z),
	deb_mon_obtention_comps(' 18)',  2,[z,x,y],[0,1,0], 2*x),
	deb_mon_obtention_comps(' 19)', -2,[z,x,y],[0,0,1], -2*y),
	deb_mon_obtention_comps(' 20)',  2,[z,x,y],[1,0,1], 2*y*z),
	deb_mon_obtention_comps(' 21)',  2,[z,x,y],[1,1,0], 2*x*z),
	deb_mon_obtention_comps(' 22)',  2,[z,x,y],[0,1,1], 2*x*y),
	deb_mon_obtention_comps(' 23)',  2,[z,x,y],[1,1,1], 2*x*y*z),
	deb_mon_obtention_comps(' 24)',  2,[z,x,y],[2,1,1], 2*x*y*z^2),
	deb_mon_obtention_comps(' 25)',  2,[z,x,y],[1,2,1], 2*x^2*y*z),
	deb_mon_obtention_comps(' 26)',  2,[z,x,y],[1,1,2], 2*x*y^2*z),
	deb_mon_obtention_comps(' 27)',  2,[z,x,y],[2,1,2], 2*x*y^2*z^2),
	deb_mon_obtention_comps(' 28)',  2,[z,x,y],[2,2,1], 2*x^2*y*z^2),
	deb_mon_obtention_comps(' 29)',  2,[z,x,y],[1,2,2], 2*x^2*y^2*z),
	deb_mon_obtention_comps(' 30)',  2,[z,x,y],[2,2,2], 2*x^2*y^2*z^2),

	write('* MONOMIAL REDUCTION'), nl,

	deb_red_mon('  1)', 0*x^0, 0),
	deb_red_mon('  2)', 0*x^1, 0),
	deb_red_mon('  3)', 0*x^5, 0),
	deb_red_mon('  4)', 1*x^0, 1),
	deb_red_mon('  5)', 1*x^1, x),
	deb_red_mon('  6)', 1*x^5, x^5),
	deb_red_mon('  7)', 3*x^0, 3),
	deb_red_mon('  8)', 3*x^1, 3*x),
	deb_red_mon('  9)', 3*x^6, 3*x^6),
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
	deb_red_mon(' 67)', 0*x^0*y^0*z^0, 0),
	deb_red_mon(' 68)', 0*x^0*y^0*z^1, 0),
	deb_red_mon(' 69)', 0*x^0*y^0*z, 0),
	deb_red_mon(' 70)', 0*x^0*y^0*z^2, 0),
	deb_red_mon(' 71)', 0*x^0*y^1*z^0, 0),
	deb_red_mon(' 72)', 0*x^0*y*z^0, 0),
	deb_red_mon(' 73)', 0*x^0*y^2*z^0, 0),
	deb_red_mon(' 74)', 0*x^0*y^2*z^1, 0),
	deb_red_mon(' 75)', 0*x^0*y^2*z^2, 0),
	deb_red_mon(' 76)', 0*x^1*y^2*z^2, 0),
	deb_red_mon(' 77)', 0*x*y^2*z^2, 0),
	deb_red_mon(' 78)', 0*x^2*y^2*z^2, 0),
	deb_red_mon(' 79)', 1*x^0*y^0*z^0, 1),
	deb_red_mon(' 80)', 2*x^0*y^0*z^1, 2*z),
	deb_red_mon(' 81)', 1*x^0*y^0*z, z),
	deb_red_mon(' 82)', 2*x^0*y^0*z^2, 2*z^2),
	deb_red_mon(' 83)', 2*x^0*y^1*z^0, 2*y),
	deb_red_mon(' 84)', 2*x^0*y*z^0, 2*y),
	deb_red_mon(' 85)', 1*x^0*y^2*z^0, y^2),
	deb_red_mon(' 86)', 2*x^0*y^2*z^1, 2*y^2*z),
	deb_red_mon(' 87)', 2*x^0*y^2*z^2, 2*y^2*z^2),
	deb_red_mon(' 88)', 1*x^1*y^2*z^2, x*y^2*z^2),
	deb_red_mon(' 89)', 2*x*y^2*z^2, 2*x*y^2*z^2),
	deb_red_mon(' 90)', 2*x^2*y^2*z^2, 2*x^2*y^2*z^2),
	deb_red_mon(' 91)', -1*x^0*y^0*z^0, -1),
	deb_red_mon(' 92)', -2*x^0*y^0*z^1, -2*z),
	deb_red_mon(' 93)', -1*x^0*y^0*z, -z),
	deb_red_mon(' 94)', -2*x^0*y^0*z^2, -2*z^2),
	deb_red_mon(' 95)', -2*x^0*y^1*z^0, -2*y),
	deb_red_mon(' 96)', -2*x^0*y*z^0, -2*y),
	deb_red_mon(' 97)', -1*x^0*y^2*z^0, -y^2),
	deb_red_mon(' 98)', -2*x^0*y^2*z^1, -2*y^2*z),
	deb_red_mon(' 99)', -2*y^0*z^2*x^2, -2*x^2*z^2),
	deb_red_mon('100)', -1*x^1*y^2*z^2, -x*y^2*z^2),
	deb_red_mon('101)', -2*z*y^2*z^2, -2*y^2*z^3),
	deb_red_mon('102)', -2*y^2*x^2*z^2, -2*x^2*y^2*z^2),
	deb_red_mon('103)', -2*z^2*x^2*z^2, -2*x^2*z^4),
	deb_red_mon('104)', -2*z^2*z^2*z^2, -2*z^6),
	deb_red_mon('105)',  1*x*y*x*y*x*y, x^3*y^3),
	deb_red_mon('106)', x*y*z*x^2*y^2*z^2*x^3*y^3*z^3, x^6*y^6*z^6),
	deb_red_mon('107)', z*y*x*z^2*x^2*y^2*x^3*z^3*y^3, x^6*y^6*z^6),

	write('* MONOMIAL COMPARISON'), nl,

	deb_mon_comparison('  1)', x, 2),
	deb_mon_comparison('  2)', 2*x, 2),
	deb_mon_comparison('  3)', x, z),
	deb_mon_comparison('  4)', x*y, z),
	deb_mon_comparison('  5)', x*z, 2),
	deb_mon_comparison('  6)', x^2, z),
	deb_mon_comparison('  7)', x*z, z^2),
	deb_mon_comparison('  8)', x*z, x^2),
	deb_mon_comparison('  9)', x*z, y),
	deb_mon_comparison(' 10)', x*z, y^2),
	deb_mon_comparison(' 11)', x^2*z, x*z),
	deb_mon_comparison(' 12)', x^2*z^2, x*z),
	deb_mon_comparison(' 13)', x^2*z^2, x^2*z),
	deb_mon_comparison(' 14)', x^2*z^2, x*z^2),
	deb_mon_comparison(' 15)', x*y^2, x^2*z^2),
	deb_mon_comparison(' 16)', x^2*y, x^2*z),
	deb_mon_comparison(' 17)', x^3*y, x^2*y),
	deb_mon_comparison(' 18)', x^3*y^3, x^3*y^2),
	deb_mon_comparison(' 19)', x^4*y^3, x^3*y^3),

	write('* MONOMIAL SORTING'), nl,

	deb_mon_sort('  1)', [x^2, y^2], [x^2, y^2]),
	deb_mon_sort('  2)', [2, x], [x, 2]),
	deb_mon_sort('  3)', [x, z^2], [x, z^2]),
	deb_mon_sort('  4)', [z^2, x], [x, z^2]),
	deb_mon_sort('  5)', [2*x, x*y], [x*y, 2*x]),
	deb_mon_sort('  6)', [y, z^2, x^3], [x^3, y, z^2]),
	deb_mon_sort('  7)', [2, y, z^2, x^3], [x^3, y, z^2, 2]),
	deb_mon_sort('  8)', [2*x, x*y, y*z^2, x^3*y], [x^3*y, x*y, y*z^2, 2*x]),

	write('* MONOMIAL SUM'), nl,

	deb_mon_sum('  1)', 3*x, 2*x, 5*x),
	deb_mon_sum('  2)', 3*x, 2*x^0, 3*x + 2),
	deb_mon_sum('  3)', 3*x, 2*x^1, 5*x),
	deb_mon_sum('  4)', 3*x^0, 2*x, 2*x + 3),
	deb_mon_sum('  5)', 3*x^1, 2*x, 5*x),
	deb_mon_sum('  6)', 3*x^2, 2*x, 3*x^2 + 2*x),
	deb_mon_sum('  7)', 3*x^2, 2*x^2, 5*x^2),
	deb_mon_sum('  8)', 3*x^0, 2*x^0, 5),
	deb_mon_sum('  9)', 3*x^0, 0*x^2, 3),
	deb_mon_sum(' 10)', 0*x^1, 0*x^2, 0),
	deb_mon_sum(' 11)', 0, 0*x^2, 0),
	deb_mon_sum(' 12)', 0, x^2, x^2),
	deb_mon_sum(' 12)', 0, 0, 0),
	deb_mon_sum(' 13)', x, y, x + y),
	deb_mon_sum(' 14)', 3*x, y, 3*x + y),
	deb_mon_sum(' 15)', 3*x, -3*y, 3*x - 3*y),
	deb_mon_sum(' 16)', 3*x, 3*y, 3*x + 3*y),
	deb_mon_sum(' 17)', 3*y, 3*x, 3*x + 3*y),
	deb_mon_sum(' 18)', 2*x^0, 2, 4),
	deb_mon_sum(' 19)', 2*x^2, 2*y, 2*x^2 + 2*y),
	deb_mon_sum(' 20)', 2*x*z, 2*x, 2*x*z + 2*x),
	deb_mon_sum(' 21)', 2*x, 2*r*x, 2*r*x + 2*x),
	deb_mon_sum(' 22)', 2, 2*x^2*y^3, 2*x^2*y^3 + 2),
	deb_mon_sum(' 23)', 2*x*y, 2*x*y, 4*x*y),
	deb_mon_sum(' 24)', 2*x*y, -2*x*y, 0),
	deb_mon_sum(' 25)', -2*x*y, 2*x*y, 0),
	deb_mon_sum(' 26)', -2*x*y, -2*x*y, -4*x*y),
	deb_mon_sum(' 27)', -1, 0, -1),
	deb_mon_sum(' 28)', 0, -1, -1),
	deb_mon_sum(' 29)', -1, -1, -2),
	deb_mon_sum(' 30)', 0, 0, 0),
	deb_mon_sum(' 31)', 1, 1, 2),
	deb_mon_sum(' 32)', 3*x, -4*x, -x),
	deb_mon_sum(' 33)', -3*x, 4*x, x),
	deb_mon_sum(' 34)', -3*x, -4*x, -7*x),

	write('* MONOMIAL SUB'), nl,

	deb_mon_sub('  1)', 0, x, -x),
	deb_mon_sub('  2)', 3*x, 2*x, x),
	deb_mon_sub('  3)', 3*x, 2*x^0, 3*x - 2),
	deb_mon_sub('  4)', 3*x, 2*x^1, x),
	deb_mon_sub('  5)', 3*x^0, 2*x, -2*x + 3),
	deb_mon_sub('  6)', 3*x^1, 2*x, x),
	deb_mon_sub('  7)', 3*x^2, 2*x, 3*x^2 - 2*x),
	deb_mon_sub('  8)', 3*x^2, 2*x^2, x^2),
	deb_mon_sub('  9)', 3*x^0, 2*x^0, 1),
	deb_mon_sub(' 10)', 3*x^0, 0*x^2, 3),
	deb_mon_sub(' 11)', 0*x^1, 0*x^2, 0),
	deb_mon_sub(' 12)', 0, 0*x^2, 0),
	deb_mon_sub(' 13)', 0, 0, 0),
	deb_mon_sub(' 14)', 0, 1/24, -1/24),
	deb_mon_sub(' 15)', 0, 1, -1),
	deb_mon_sub(' 16)', 3*x, 3*y, 3*x - 3*y),
	deb_mon_sub(' 17)', 3*y, 3*x, -3*x + 3*y),
	deb_mon_sub(' 18)', 3*y, -3*x, 3*x + 3*y),
	deb_mon_sub(' 19)', -3*y, 3*x, -3*x - 3*y),
	deb_mon_sub(' 20)', -3*y, -3*x, 3*x - 3*y),
	deb_mon_sub(' 21)', -3*y*z, -3*x, -3*y*z + 3*x),
	deb_mon_sub(' 22)', -3*asdf, -3*x, -3*asdf + 3*x),

	write('* MONOMIAL PROD'), nl,

	deb_mon_prod('  1)', 3*x, 2*x, 6*x^2),
	deb_mon_prod('  2)', 3*x, 2*x^0, 6*x),
	deb_mon_prod('  3)', 3*x, 2*x^1, 6*x^2),
	deb_mon_prod('  4)', 3*x^0, 2*x, 6*x),
	deb_mon_prod('  5)', 3*x^1, 2*x, 6*x^2),
	deb_mon_prod('  6)', 3*x^2, 2*x, 6*x^3),
	deb_mon_prod('  7)', 3*x^2, 2*x^2, 6*x^4),
	deb_mon_prod('  8)', 3*x^0, 2*x^0, 6),
	deb_mon_prod('  9)', 3*x^0, 0*x^2, 0),
	deb_mon_prod(' 10)', 0*x^1, 0*x^2, 0),
	deb_mon_prod(' 11)', 0, 0*x^2, 0),
	deb_mon_prod(' 12)', 0, 0, 0),
	deb_mon_prod(' 13)', x, y, x*y),
	deb_mon_prod(' 14)', 3*x, y, 3*x*y),
	deb_mon_prod(' 15)', 3*x, 3*y, 9*x*y),
	deb_mon_prod(' 16)', y, x, x*y),
	deb_mon_prod(' 17)', 3*y, x, 3*x*y),
	deb_mon_prod(' 18)', 3*y, 3*x, 9*x*y),
	deb_mon_prod(' 19)', 3*y^0, -4*x, -12*x),
	deb_mon_prod(' 20)', 3*y, -4*x, -12*x*y),
	deb_mon_prod(' 21)', 3*y, -4*x^0, -12*y),
	deb_mon_prod(' 22)', 3*y^0, -4*x^0, -12),
	deb_mon_prod(' 23)', 3*x*z, -4*x*y, -12*x^2*y*z),
	deb_mon_prod(' 24)', 3*a*z, 3*b*y, 9*a*b*y*z),
	deb_mon_prod(' 25)', 3, x*y, 3*x*y),
	deb_mon_prod(' 26)', x*y, 3, 3*x*y),
	deb_mon_prod(' 27)', 0*x*y, x*y, 0),
	deb_mon_prod(' 28)', -1, x*y, -x*y),
	deb_mon_prod(' 29)', -1, 0*z, 0),
	deb_mon_prod(' 30)', z, y, y*z),
	deb_mon_prod(' 31)', z, -1, -z),

	write('* MONOMIAL POSITIVE COEFFICIENT'), nl,

	deb_mon_pos_coef('  1)', 3*x, 'YES'),
	deb_mon_pos_coef('  2)', -3*x, 'NO'),
	deb_mon_pos_coef('  3)', -x, 'NO'),
	deb_mon_pos_coef('  4)', x, 'YES'),

	write('* MONOMIAL EVALUATION'), nl,

	deb_mon_eval('  1)',	     1, x, 0, 1),
	deb_mon_eval('  2)',         1, x, 1, 1),
	deb_mon_eval('  3)',         1, x, 100, 1),
	deb_mon_eval('  4)',        -1, x, 100, -1),
	deb_mon_eval('  5)',        -1, x, -2, -1),
	deb_mon_eval('  6)',        -x, x, -2, 2),
	deb_mon_eval('  7)',         x, x, 2, 2),
	deb_mon_eval('  8)',       x^2, x, 2, 4),
	deb_mon_eval('  9)',       3*x, x, 2, 6),
	deb_mon_eval(' 10)',     3*x^2, x, 2, 12),
	deb_mon_eval(' 11)',     9*x^2, x, 2, 36),
	deb_mon_eval(' 12)',      -9*x, x, 2, -18),
	deb_mon_eval(' 13)',   -10*x^3, x, 0, 0),
	deb_mon_eval(' 14)',       x^3, x, 0, 0),
	deb_mon_eval(' 15)',         0, x, 11, 0),
	deb_mon_eval(' 16)',	   3*x, i, 11, 3*x),
	deb_mon_eval(' 17)',         0, i, 11, 0),
	deb_mon_eval(' 18)',       3*i, i, 3, 9),
	deb_mon_eval(' 19)',     3*x*y, x, 3, 9*y),
	deb_mon_eval(' 20)',   3*x^2*y, x, 3, 27*y),
	deb_mon_eval(' 21)',   3*x^2*y, y, 3, 9*x^2),
	deb_mon_eval(' 22)',   3*x^2*y, z, 3, 3*x^2*y),
	deb_mon_eval(' 23)', 3*x^2*y*z, z, 0, 0),
	deb_mon_eval(' 24)', 3*x^2*y*z, z, 1, 3*x^2*y),

	write('* MONOMIAL REVAR'), nl,

	deb_mon_revar('  1)', x,y, 3*x,	    3*y),
	deb_mon_revar('  2)', z,x, 3*x*y*z, 3*x^2*y),
	deb_mon_revar('  3)', z,y, 3*x*y*z, 3*x*y^2),
	deb_mon_revar('  4)', a,x, 3*x*y,   3*x*y),
	deb_mon_revar('  5)', x,y, 3,       3),

	write('* MONOMIAL SPLIT'), nl,

	deb_mon_split('  1)', x,       3*x,   x, 3),
	deb_mon_split('  2)', z,       3*x,   1, 3*x),
	deb_mon_split('  3)', y, 3*x*y^2*z, y^2, 3*x*z),
	deb_mon_split('  4)', y,   3*x^2*z,   1, 3*x^2*z),
	deb_mon_split('  5)', z,   3*x^2*z,   z, 3*x^2),
	deb_mon_split('  6)', x,   3*x^2*z, x^2, 3*z),

	nl, true.

/*
-----------------------------
DEBUG - POLYNOMIAL EVALUATION
*/

deb_poly_list_sum(I, P1, P2, RES):-
	polynomial_from_list_sum_list(P1, P2, R), polynomial_list_eq(RES, R), !, output_correct(I).
deb_poly_list_sum(I, P1, P2, RES):-
	polynomial_from_list_sum_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' + '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_sorted_list_sum(I, P1, P2, RES):-
	unipoly_from_list_sum_sorted_list(P1, P2, R), polynomial_list_eq(RES, R), !, output_correct(I).
deb_poly_sorted_list_sum(I, P1, P2, RES):-
	unipoly_from_list_sum_sorted_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' + '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_list_sub(I, P1, P2, RES):-
	polynomial_from_list_sub_list(P1, P2, R), polynomial_list_eq(RES, R), !, output_correct(I).
deb_poly_list_sub(I, P1, P2, RES):-
	polynomial_from_list_sub_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' - '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_sorted_list_sub(I, P1, P2, RES):-
	unipoly_from_list_sub_sorted_list(P1, P2, R), polynomial_list_eq(RES, R), !, output_correct(I).
deb_poly_sorted_list_sub(I, P1, P2, RES):-
	unipoly_from_list_sub_sorted_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' - '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_list_prod(I, P1, P2, RES):-
	polynomial_from_list_prod_list(P1, P2, R), polynomial_list_eq(RES, R), !, output_correct(I).
deb_poly_list_prod(I, P1, P2, RES):-
	polynomial_from_list_prod_list(P1, P2, R),
	write(I), write(' '), write(P1), write(' * '), write(P2), write(' = '),
	output_text(R, RES).

deb_poly_sorted_list_prod(I, P1, P2):-
	unipoly_from_list_prod_sorted_list(P1, P2, R1),
	polynomial_from_list_prod_list(P1, P2, R2),
	polynomial_list_eq(R1, R2), !, output_correct(I).
deb_poly_sorted_list_prod(I, P1, P2):-
	unipoly_from_list_prod_sorted_list(P1, P2, R1),
	polynomial_from_list_prod_list(P1, P2, R2), write(I), write(' '), write(P1),
	write(' * '), write(P2), write(' = '), output_text(R1, R2).

deb_poly_sum(I, P, RES):- polynomial_reduction(P, R), polynomial_eq(R, RES), !, output_correct(I).
deb_poly_sum(I, P, RES):-
	polynomial_reduction(P, R),
	write(I), write(' '), write(P), write(' = '), output_text(R, RES).

deb_poly_prod(I, P1, P2, RES):-
	polynomial_prod(P1, P2, R), polynomial_eq(R, RES), !, output_correct(I).
deb_poly_prod(I, P1, P2, RES):-
	polynomial_prod(P1, P2, R),
	write(I), write(' ('), write(P1), write(')*('), write(P2), write(') = '),
	output_text(R, RES).

deb_poly_pow(I, P, N, RES):- polynomial_power(P, N, R), polynomial_eq(R, RES), !, output_correct(I).
deb_poly_pow(I, P, N, RES):-
	polynomial_power(P, N, R),
	write(I), write(' ('), write(P), write(')^'), write(N), write(' = '),
	output_text(R, RES).

deb_poly_eval(I, E, RES):- polynomial_evaluation(E, R), polynomial_eq(R, RES), !, output_correct(I).
deb_poly_eval(I, E, RES):-
	polynomial_evaluation(E, R),
	write(I), write(' '), write(E), write(' = '), output_text(R, RES).

deb_mono_eval(I, M,V, E, RES):-
	monomial_symb_evaluation(V, E, M, RES), !, output_correct(I).
deb_mono_eval(I, M,V, E, RES):-
	monomial_symb_evaluation(V, E, M, N),
	write(I), write(' '),
	write(M), write( '(-> '), write(E), write(' @ '), write(V), write(' )= '),
	output_text(N, RES).

deb_poly_first_mon(I, E, RES1, RES2):-
	polynomial_first_monomial(E, R1, R2), polynomial_eq(R1, RES1),
	polynomial_eq(R2, RES2), !, output_correct(I).
deb_poly_first_mon(I, E, RES1, RES2):-
	polynomial_first_monomial(E, R1, R2),
	write(I), write('' ), write(E), write(' -> '),
	output_text( (R1, R2), (RES1, RES2)).

deb_poly_last_mon(I, E, RES1, RES2):-
	polynomial_last_monomial(E, R1, R2), polynomial_eq(R1, RES1),
	polynomial_eq(R2, RES2), !, output_correct(I).
deb_poly_last_mon(I, E, RES1, RES2):-
	polynomial_last_monomial(E, R1, R2),
	write(I), write('' ), write(E), write(' -> '),
	output_text( (R1, R2), (RES1, RES2)).

deb_poly_pad(I, L, RES):- padded_unipoly_mons_decr(L, R), R == RES, !, output_correct(I).
deb_poly_pad(I, L, RES):-
	padded_unipoly_mons_decr(L, R),
	write(I), write(' '), write(L), write(' -> '), output_text(R, RES).

deb_pretty_polynomial_roots(I, R, RES):-
	pretty_polynomial_roots(R, P), polynomial_eval_eq(RES, P), !, output_correct(I).
deb_pretty_polynomial_roots(I, R, RES):-
	pretty_polynomial_roots(R, P),
	write(I), write(' Using roots '), write(R), write(' -> '), output_text(P, RES).

deb_poly_roots(I, P, RES):-
	polynomial_evaluation(P, E), integer_roots_polynomial(E, R),
	msort(R, SR), msort(RES, SRES), SR == SRES, !, output_correct(I).
deb_poly_roots(I, P, RES):-
	polynomial_evaluation(P, E), integer_roots_polynomial(E, R),
	write(I), write(' '), write(P), write(' -> '), output_text(R, RES).

deb_poly_roots_eval_roots(I, R):-
	pretty_polynomial_roots(R, P), polynomial_evaluation(P, Q),
	integer_roots_polynomial(Q, RES), sort(R, RS), sort(RES, RESS),
	RS == RESS, !, output_correct(I).
deb_poly_roots_eval_roots(I, R):-
	write(I), write(' '),
	pretty_polynomial_roots(R, P),
	polynomial_evaluation(P, Q),
	integer_roots_polynomial(Q, L),
	all_to_string([R, space, ->, space, P, space, ->, space, Q, space, ->, space, L], S1),
	all_to_string([R, space, ->, space, P, space, ->, space, Q, space, ->, space, R], S2),
	output_text(S1, S2), nl.

deb_exp_poly_eval(I, P, V, VAL, RES):-
	expanded_polynomial_evaluation(VAL, V, P, RES), !, output_correct(I).
deb_exp_poly_eval(I, P, V, VAL, RES):-
	expanded_polynomial_evaluation(VAL, V, P, R),
	write(I), write(' P('), write(V), write(')='), write(P), write(' -> P('), write(VAL),
	write(')= '), output_text(R, RES).

deb_falling_factorial(I, P, F, RES):-
	polynomial_evaluation(choose(P, F), R), polynomial_evaluation(RES, R), !, output_correct(I).
deb_falling_factorial(I, P, F, RES):-
	polynomial_evaluation(choose(P, F), R),
	write(I), write(' choose('), write(P), write(','), write(F), write(')= '), write(R),
	polynomial_evaluation(RES, CORRECT_OUTPUT), output_text(R, CORRECT_OUTPUT).

debug_polynomials:-
	write('-- POLYNOMIAL EVALUATION DEBUG --'), nl,
	write('* POLYNOMIAL PADDING'), nl,

	deb_poly_pad('  1)', [2], [2]),
	deb_poly_pad('  2)', [x], [x, 0]),
	deb_poly_pad('  3)', [x, 3], [x, 3]),
	deb_poly_pad('  4)', [x^2], [x^2, 0, 0]),
	deb_poly_pad('  5)', [x^2, 3], [x^2, 0, 3]),
	deb_poly_pad('  6)', [x^3, x], [x^3, 0, x, 0]),
	deb_poly_pad('  7)', [x^4, x^3], [x^4, x^3, 0, 0, 0]),
	deb_poly_pad('  8)', [x^4, x^3, x], [x^4, x^3, 0, x, 0]),
	deb_poly_pad('  9)', [x^4, x^2, 1], [x^4, 0, x^2, 0, 1]),
	deb_poly_pad(' 10)', [x^3], [x^3, 0, 0, 0]),
	deb_poly_pad(' 11)', [x^4], [x^4, 0, 0, 0, 0]),
	deb_poly_pad(' 12)', [x^6, x^4], [x^6, 0, x^4, 0, 0, 0, 0]),

	write('* POLYNOMIAL LIST SUM'), nl,

	deb_poly_list_sum('  1)', [], [], []),
	deb_poly_list_sum('  2)', [], [0], []),
	deb_poly_list_sum('  3)', [0], [], []),
	deb_poly_list_sum('  4)', [0], [0], []),
	deb_poly_list_sum('  5)', [1], [0], [1]),
	deb_poly_list_sum('  6)', [0], [1], [1]),
	deb_poly_list_sum('  7)', [1], [1], [2]),
	deb_poly_list_sum('  8)', [x], [0], [x]),
	deb_poly_list_sum('  9)', [x], [x], [2*x]),
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
	deb_poly_list_sum(' 21)', [x,z], [], [x,z]),
	deb_poly_list_sum(' 22)', [x,z], [x,z], [2*x,2*z]),
	deb_poly_list_sum(' 23)', [x^2,x,z], [x,y,z], [x^2,2*x,y,2*z]),
	deb_poly_list_sum(' 24)', [x^2,z], [-x^2, -z], []),
	deb_poly_list_sum(' 25)', [-x^2,z^2], [x^2,-z^2], []),
	deb_poly_list_sum(' 26)', [x,y,z], [x,y,z], [2*x,2*y,2*z]),
	deb_poly_list_sum(' 27)', [x^2,y^2,x^2,z^2], [x,x,x,y,y,y,z^2], [2*x^2, y^2, 2*z^2, 3*x,3*y]),

	write('* POLYNOMIAL SORTED LIST SUM'), nl,

	deb_poly_sorted_list_sum('  1)', [], [], []),
	deb_poly_sorted_list_sum('  2)', [0], [0], []),
	deb_poly_sorted_list_sum('  5)', [1], [0], [1]),
	deb_poly_sorted_list_sum('  6)', [0], [1], [1]),
	deb_poly_sorted_list_sum('  7)', [1], [1], [2]),
	deb_poly_sorted_list_sum('  8)', [x], [0], [x]),
	deb_poly_sorted_list_sum('  9)', [x], [x], [2*x]),
	deb_poly_sorted_list_sum(' 10)', [x], [-x], []),
	deb_poly_sorted_list_sum(' 11)', [2*x], [-x], [x]),
	deb_poly_sorted_list_sum(' 12)', [-2*x], [-x], [-3*x]),
	deb_poly_sorted_list_sum(' 13)', [3*x], [-4*x], [-x]),
	deb_poly_sorted_list_sum(' 14)', [x^2], [-x], [x^2, -x]),
	deb_poly_sorted_list_sum(' 15)', [4*x^3], [-x], [4*x^3, -x]),
	deb_poly_sorted_list_sum(' 16)', [x^3, x^2, x, 1], [-x], [x^3, x^2, 1]),
	deb_poly_sorted_list_sum(' 17)', [x^3, x^2, x, -1], [x], [x^3, x^2, 2*x, -1]),

	write('* POLYNOMIAL LIST SUB'), nl,

	deb_poly_list_sub('  1)', [], [], []),
	deb_poly_list_sub('  2)', [], [0], []),
	deb_poly_list_sub('  3)', [0], [], []),
	deb_poly_list_sub('  4)', [0], [0], []),
	deb_poly_list_sub('  5)', [1], [0], [1]),
	deb_poly_list_sub('  6)', [0], [1], [-1]),
	deb_poly_list_sub('  7)', [1], [1], []),
	deb_poly_list_sub('  8)', [x], [0], [x]),
	deb_poly_list_sub('  9)', [x], [x], []),
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

	write('* POLYNOMIAL SORTED LIST SUB'), nl,

	deb_poly_sorted_list_sub('  1)', [], [], []),
	deb_poly_sorted_list_sub('  2)', [0], [0], []),
	deb_poly_sorted_list_sub('  3)', [1], [0], [1]),
	deb_poly_sorted_list_sub('  4)', [0], [1], [-1]),
	deb_poly_sorted_list_sub('  5)', [1], [1], []),
	deb_poly_sorted_list_sub('  6)', [x], [0], [x]),
	deb_poly_sorted_list_sub('  7)', [x], [x], []),
	deb_poly_sorted_list_sub('  8)', [x], [-x], [2*x]),
	deb_poly_sorted_list_sub('  9)', [2*x], [-x], [3*x]),
	deb_poly_sorted_list_sub(' 10)', [-2*x], [-x], [-x]),
	deb_poly_sorted_list_sub(' 11)', [3*x], [-4*x], [7*x]),
	deb_poly_sorted_list_sub(' 12)', [x^2], [-x], [x^2, x]),
	deb_poly_sorted_list_sub(' 13)', [4*x^3], [-x], [4*x^3, x]),
	deb_poly_sorted_list_sub(' 14)', [x^3, x^2, x, 1], [-x], [x^3, x^2, 2*x, 1]),
	deb_poly_sorted_list_sub(' 15)', [x^3, x^2, x, -1], [x], [x^3, x^2, -1]),
	deb_poly_sorted_list_sub(' 16)', [x^3, x^2, x, -1], [(1/2)*x^3, x], [(1/2)*x^3, x^2, -1]),

	write('* POLYNOMIAL LIST PROD'), nl,

	deb_poly_list_prod('  1)', [], [], []),
	deb_poly_list_prod('  2)', [], [0], []),
	deb_poly_list_prod('  3)', [0], [], []),
	deb_poly_list_prod('  4)', [0], [0], []),
	deb_poly_list_prod('  5)', [1], [0], []),
	deb_poly_list_prod('  6)', [0], [1], []),
	deb_poly_list_prod('  7)', [1], [1], [1]),
	deb_poly_list_prod('  8)', [x], [0], []),
	deb_poly_list_prod('  9)', [x], [x], [x^2]),
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

	write('* POLYNOMIAL SORTED LIST PROD'), nl,

	deb_poly_sorted_list_prod('  1)', [], []),
	deb_poly_sorted_list_prod('  2)', [x], []),
	deb_poly_sorted_list_prod('  3)', [x], [x]),
	deb_poly_sorted_list_prod('  4)', [x^2], [x]),
	deb_poly_sorted_list_prod('  5)', [x], [x, 1]),
	deb_poly_sorted_list_prod('  6)', [x^3, x], [x^2, 1]),
	deb_poly_sorted_list_prod('  7)', [x^3, x^2, x, 1], [x, -3]),
	deb_poly_sorted_list_prod('  8)', [4*x^4, x^2, -34], [x^3, -2*x^2]),
	deb_poly_sorted_list_prod('  9)', [4*x^3, x^2, -34], [x^3, -2*x^2]),
	deb_poly_sorted_list_prod(' 10)', [x^20, -x^15, 6*x^12, -24*x^8, 4*x^3, x^2, -34], [x^10, x^5, x^3, -2*x^2]),

	write('* POLYNOMIAL REDUCTION'), nl,

	deb_poly_sum('  1)', 0*x^0, 0),
	deb_poly_sum('  2)', 0 + x + 0, x),
	deb_poly_sum('  3)', 0 + x^2 + x^(1 + 1), 2*x^2),
	deb_poly_sum('  4)', x^2 - 2*x^(3 - 1), -x^2),
	deb_poly_sum('  5)', x^2 - 2*x^(3 - 1), -x^2),
	deb_poly_sum('  6)', (1/2)*x - (1/2)*x, 0),
	deb_poly_sum('  7)', (1/2)*x + x^2 - (1/2)*x, x^2),
	deb_poly_sum('  8)', (1/2)*x - (3/2)*x, -x),
	deb_poly_sum('  9)', 0 + (1/2)*x + (3/2)*x, 2*x),
	deb_poly_sum(' 10)', 0 + (1/2)*x - 0 + (3/2)*x, 2*x),
	deb_poly_sum(' 11)', x^2 + (1/2)*x - 0 + (3/2)*x, 2*x + x^2),
	deb_poly_sum(' 12)', x - x^2 + x^3 - x^4 + x^5, x - x^2 + x^3 - x^4 + x^5),
	deb_poly_sum(' 13)', (1/3)*x^4 + (1/2)*x^3 + (1/6)*x^2 - (1/6)*x^3 + (1/4)*x^2 - (1/12)*x - (1/12)*x^2 + (1/12)*x - x^3, (1/3)*x^2 - (2/3)*x^3 + (1/3)*x^4),
	deb_poly_sum(' 14)', x^2 + y, x^2 + y),
	deb_poly_sum(' 15)', x^2 + y + x^2, 2*x^2 + y),
	deb_poly_sum(' 16)', x^2 + y + x, x^2 + x + y),
	deb_poly_sum(' 17)', -z - y + x^2 + y + x, x^2 + x - z),
	deb_poly_sum(' 18)', -z - y + x^2 + y + x + z, x^2 + x),
	deb_poly_sum(' 19)', z^2 + x - y - 3*x + 4*z^2 + 2*y, 5*z^2 + y - 2*x),
	deb_poly_sum(' 20)', a + z^2 + x - y - 3*x + 4*z^2 + 2*y - a, 5*z^2 - 2*x + y),

	write('* POLYNOMIAL PRODUCT'), nl,

	deb_poly_prod('  1)', x, x, x^2),
	deb_poly_prod('  2)', x^2, x, x^3),
	deb_poly_prod('  3)', x + 1, x, x^2 + x),
	deb_poly_prod('  4)', x, x + 1, x^2 + x),
	deb_poly_prod('  5)', x + 1, x + 1, x^2 + 2*x + 1),
	deb_poly_prod('  5)', x^2 + 1, x + 1, x^3 + x^2 + x + 1),
	deb_poly_prod('  6)', x^2 + 2, 6*x + 1, 6*x^3 + x^2 + 12*x + 2),
	deb_poly_prod('  7)', x^2 + 2*x^2, -6*x - 1, -18*x^3 - 3*x^2),
	deb_poly_prod('  8)', x^2 + 0, -6*x - 1, -6*x^3 - x^2),
	deb_poly_prod('  9)', -1 + x^2 + 1, -6*x + 1, -6*x^3 + x^2),
	deb_poly_prod(' 10)', -1 + x^2 + 1, -1 + x^2 + 1, x^4),
	deb_poly_prod(' 11)', 2*x^2 - 3*x^3 + 30, 2*x^2 - 3*x^3 + 30, 9*x^6 - 12*x^5 + 4*x^4 - 180*x^3 + 120*x^2 + 900),
	deb_poly_prod(' 12)', 0, x, 0),
	deb_poly_prod(' 13)', x, z, x*z),
	deb_poly_prod(' 14)', x + 1, z, x*z + z),
	deb_poly_prod(' 15)', x + y, x^2 + z, x^2*y + x^3 + x*z + y*z),
	deb_poly_prod(' 16)', x*y*z + x^2 + n^2, 3*k^2 + 4*q, 4*q*x*y*z + 4*q*x^2 + 4*q*n^2 + 3*k^2*x*y*z + 3*k^2*x^2 + 3*k^2*n^2),

	write('* POLYNOMIAL POWER'), nl,

	deb_poly_pow('  1)', x, 0, 1),
	deb_poly_pow('  2)', x + 1, 0, 1),
	deb_poly_pow('  3)', x + 1, 1, x + 1),
	deb_poly_pow('  4)', x - 1, 1, x - 1),
	deb_poly_pow('  5)', x - 1, 2, x^2 - 2*x + 1),
	deb_poly_pow('  6)', x + 1, 2, x^2 + 2*x + 1),
	deb_poly_pow('  7)', 2*x + 1, 2, 4*x^2 + 4*x + 1),
	deb_poly_pow('  8)', 2*x^2, 2, 4*x^4),
	deb_poly_pow('  9)', x^2, 2, x^4),
	deb_poly_pow(' 10)', x + 1, 3, x^3 + 3*x^2 + 3*x + 1),
	deb_poly_pow(' 11)', 2*x^2 - 3*x^3 + 30, 1, 2*x^2 - 3*x^3 + 30),
	deb_poly_pow(' 12)', 2*x^2 - 3*x^3 + 30, 2, 9*x^6 - 12*x^5 + 4*x^4 - 180*x^3 + 120*x^2 + 900),
	deb_poly_pow(' 13)', 2*x^2 - 3*x^3 + 30, 3, -27*x^9 + 54*x^8 - 36*x^7 + 818*x^6 - 1080*x^5 + 360*x^4 - 8100*x^3 + 5400*x^2 + 27000),
	deb_poly_pow(' 14)', 2*x^2 - 3*x^3 + 30, 4, 81*x^12 - 216*x^11 + 216*x^10 - 3336*x^9 + 6496*x^8 - 4320*x^7 + 49560*x^6 - 64800*x^5 + 21600*x^4 - 324000*x^3 + 216000*x^2 + 810000),
	deb_poly_pow(' 15)', 2*x^2 - 3*x^3 + 30, 5, -243*x^15 + 810*x^14 - 1080*x^13 + 12870*x^12 - 32640*x^11 + 32432*x^10 - 257400*x^9 + 488400*x^8 - 324000*x^7 + 2502000*x^6 - 3240000*x^5 + 1080000*x^4 - 12150000*x^3 + 8100000*x^2 + 24300000),
	deb_poly_pow(' 16)', 2 + 2, 2, 16),
	deb_poly_pow(' 17)', 0, 2, 0),
	deb_poly_pow(' 18)', (x + z), 2, x^2 + 2*x*z + z^2),
	deb_poly_pow(' 19)', (x + z + y), 2, x^2 + y^2 + z^2 + 2*x*z + 2*x*y + 2*y*z),
	deb_poly_pow(' 20)', (x + z + 5), 2, x^2 + 25 + z^2 + 2*x*z + 10*x + 10*z),
	deb_poly_pow(' 21)', (x + z + 1/4), 2, x^2 + 1/16 + z^2 + 2*x*z + 1/2*x + 1/2*z),
	deb_poly_pow(' 22)', (x + 1/4 + z), 2, x^2 + 1/16 + z^2 + 2*x*z + 1/2*x + 1/2*z),
	deb_poly_pow(' 23)', (1/4 + x + z), 2, x^2 + 1/16 + z^2 + 2*x*z + 1/2*x + 1/2*z),

	write('* POLYNOMIAL SYMBOLIC EVALUATION'), nl,

	deb_poly_eval('  1)', x + x - 2, 2*x - 2),
	deb_poly_eval('  2)', x + x^2 - 2, x^2 + x - 2),
	deb_poly_eval('  3)', x*x^2 - 2, x^3 - 2),
	deb_poly_eval('  4)', (x + 1)^2 - 1, x^2 + 2*x),
	deb_poly_eval('  5)', x*((x + 1)^2) - 1, x^3 + 2*x^2 + x - 1),
	deb_poly_eval('  6)', x*(x + 1)^2 - x, x^3 + 2*x^2),
	deb_poly_eval('  7)', (1/8)*x*((x + 1)^2)*(2*x + 1) - (1/16)*x*(x + 1)*(2*x + 1) - (1/16)*x*(x + 1), 1/4*x^4 + 1/2*x^3 + 1/4*x^2),
	deb_poly_eval('  8)', (-1/8)*(n^2 + n)^2 + (1/12)*n*(n - 2)*(n + 1)*(2*n + 1) + (1/4)*n*(n - 1)*(n + 1), 1/24*n^4 - 1/12*n^3 - 13/24*n^2 - 5/12*n),
	deb_poly_eval('  9)', (3*x)^5, 243*x^5),
	deb_poly_eval(' 10)', 3*x^5, 3*x^5),

	% Attention: in swi-prolog:
	% -(z + 1)^2 = (-(z + 1))^2 != -((z + 1)^2)

	deb_poly_eval(' 11)', -((z + 1)^2) + (x + 1)^2 + (z + 1)^2, x^2 + 2*x + 1),
	deb_poly_eval(' 12)', (z + 1)^2 + (x + 1)^2 + (z + 1)^2, x^2 + 2*x + 2*z^2 + 4*z + 3),
	deb_poly_eval(' 13)', (-z + 1)^2 + (x + 1)^2, x^2 + 2*x + z^2 - 2*z + 2),
	deb_poly_eval(' 14)', 2*z + (-z + 1)^2 + (x + 1)^2, x^2 + 2*x + z^2 + 2),
	deb_poly_eval(' 15)', 3*x*(y + z), 3*x*y + 3*x*z),
	deb_poly_eval(' 16)', 3*x^2*(y + z)^2, 3*x^2*y^2 + 3*x^2*z^2 + 6*x^2*y*z),
	deb_poly_eval(' 17)', 3*x^2*(y + z)^2, 3*x^2*y^2 + 3*x^2*z^2 + 6*x^2*y*z),
	deb_poly_eval(' 18)', n*(x + y + z + n + 3), n*x + n*y + n*z + n^2 + 3*n),

	write('* MONOMIAL SYMBOLIC EVALUATION'), nl,

	deb_mono_eval('  1)', 3*i,i,   n, 3*n),
	deb_mono_eval('  2)', 3*i^2,i, n, 3*n^2),
	deb_mono_eval('  3)', 3*i^0,i, n, 3),
	deb_mono_eval('  4)', 3*i^0,i, n-2, 3),
	deb_mono_eval('  5)', 3*i,i,   n-2, 3*n - 6),
	deb_mono_eval('  6)', 3*i^2,i, n-2, 3*n^2 - 12*n + 12),
	deb_mono_eval('  7)', 3*i^2,i, 0, 0),
	deb_mono_eval('  8)', 3*i^2,i, 3, 27),
	deb_mono_eval('  9)', 3*i,j,   n, 3*i),
	deb_mono_eval(' 10)', 3*i^2,j, n, 3*i^2),
	deb_mono_eval(' 11)', 3*i^0,j, n, 3),
	deb_mono_eval(' 12)', 3*i^0,j, n-2, 3),
	deb_mono_eval(' 13)', 3*i,j,   n-2, 3*i),
	deb_mono_eval(' 14)', 3*i^2,j, n-2, 3*i^2),
	deb_mono_eval(' 15)', 3*i^2,j, 0, 3*i^2),
	deb_mono_eval(' 16)', 3*i^2,j, 3, 3*i^2),
	deb_mono_eval(' 17)', 3*i*j^2*k^3*l^4,i, 0,   0),
	deb_mono_eval(' 18)', 3*i*j^2*k^3*l^4,i, 3,   9*j^2*k^3*l^4),
	deb_mono_eval(' 19)', 3*i*j^2*k^3*l^4,i, n,   3*j^2*k^3*l^4*n),
	deb_mono_eval(' 20)', 3*i*j^2*k^3*l^4,i, n-2, 3*j^2*k^3*l^4*n - 6*j^2*k^3*l^4),
	deb_mono_eval(' 21)', 3*i*j^2*k^3*l^4,j, 0,   0),
	deb_mono_eval(' 22)', 3*i*j^2*k^3*l^4,j, 3,   27*i*k^3*l^4),
	deb_mono_eval(' 23)', 3*i*j^2*k^3*l^4,j, n,   3*i*k^3*l^4*n^2),
	deb_mono_eval(' 24)', 3*i*j^2*k^3*l^4,j, n-2, 3*i*k^3*l^4*n^2 - 12*i*k^3*l^4*n + 12*i*k^3*l^4),
	deb_mono_eval(' 25)', 3*i*j^2*k^3*l^4,k, 0,   0),
	deb_mono_eval(' 26)', 3*i*j^2*k^3*l^4,k, 3,   81*i*j^2*l^4),
	deb_mono_eval(' 27)', 3*i*j^2*k^3*l^4,k, n,   3*i*j^2*l^4*n^3),
	deb_mono_eval(' 28)', 3*i*j^2*k^3*l^4,k, n-2, 3*i*j^2*l^4*n^3 - 18*i*j^2*l^4*n^2 + 36*i*j^2*l^4*n - 24*i*j^2*l^4),
	deb_mono_eval(' 29)', 3*i*j^2*k^3*l^4,l, 0,   0),
	deb_mono_eval(' 30)', 3*i*j^2*k^3*l^4,l, 3,   243*i*j^2*k^3),
	deb_mono_eval(' 31)', 3*i*j^2*k^3*l^4,l, n,   3*i*j^2*k^3*n^4),
	deb_mono_eval(' 32)', 3*i*j^2*k^3*l^4,l, n-2, 3*i*j^2*k^3*n^4 - 24*i*j^2*k^3*n^3 + 72*i*j^2*k^3*n^2 - 96*i*j^2*k^3*n + 48*i*j^2*k^3),

	write('* POLYNOMIAL\' FIRST AND LAST MONOMIALS'), nl,

	deb_poly_first_mon('  1.1)', 3*x - 2*x^2,	3*x,		-2*x^2),
	deb_poly_first_mon('  1.2)', 3*x + 2*x^2,	3*x,		2*x^2),
	deb_poly_first_mon('  1.3)', 3*x - 2*x^3 + 4*x^4,	3*x,	-2*x^3 + 4*x^4),
	deb_poly_first_mon('  1.4)', 3*x + 2*x^2 + 4*x^3,	3*x,	2*x^2 + 4*x^3),
	deb_poly_first_mon('  1.5)', 3*x + 2*x^2 + 4*x^3,	3*x,	2*x^2 + 4*x^3),
	deb_poly_first_mon('  1.6)', a + 3*x + 2*x^2 + 4*x^3,	a,	3*x + 2*x^2 + 4*x^3),
	deb_poly_first_mon('  1.7)', 3*x + a + 2*x^2 + 4*x^3,	3*x,	a + 2*x^2 + 4*x^3),
	deb_poly_first_mon('  1.8)', 3*x + 2*x^2 + a + 4*x^3,	3*x,	a + 2*x^2 + 4*x^3),
	deb_poly_first_mon('  1.9)', z + 3*x + 2*x^2 + a + 4*x^3,	z,	3*x + a + 2*x^2 + 4*x^3),
	deb_poly_first_mon(' 1.10)', -z + 3*x + 2*x^2 + a + 4*x^3,	-z,	3*x + a + 2*x^2 + 4*x^3),

	deb_poly_last_mon('  2.1)', 3*x - 2*x^2,	3*x,		-2*x^2),
	deb_poly_last_mon('  2.2)', 3*x + 2*x^2,	3*x,		2*x^2),
	deb_poly_last_mon('  2.3)', 3*x - 2*x^3 + 4*x^4,	3*x - 2*x^3,	4*x^4),
	deb_poly_last_mon('  2.4)', 3*x + 2*x^2 + 4*x^3,	3*x + 2*x^2,	4*x^3),
	deb_poly_last_mon('  2.5)', 3*x + 2*x^2 + 4*x^3 + a,	3*x + 2*x^2 + 4*x^3,	a),
	deb_poly_last_mon('  2.6)', 3*x + 2*x^2 + a + 4*x^3,	3*x + 2*x^2 + a,	4*x^3),
	deb_poly_last_mon('  2.7)', z + 3*x + 2*x^2 + a + 4*x^3,	z + 3*x + 2*x^2 + a,	4*x^3),
	deb_poly_last_mon('  2.8)', 3*x + 2*x^2 + a + 4*x^3 - z,	3*x + 2*x^2 + a + 4*x^3,-z),

	write('* PRETTY POLYNOMIAL ROOTS'), nl,

	deb_pretty_polynomial_roots('  1)', [2], (x - 2)),
	deb_pretty_polynomial_roots('  2)', [2,3], (x - 2)*(x - 3)),
	deb_pretty_polynomial_roots('  3)', [2,3,4], (x - 2)*(x - 3)*(x - 4)),
	deb_pretty_polynomial_roots('  4)', [2,3,4,5], (x - 2)*(x - 3)*(x - 4)*(x - 5)),
	deb_pretty_polynomial_roots('  5)', [2,2], (x - 2)^2),
	deb_pretty_polynomial_roots('  6)', [2,2,3], ((x - 2)^2)*(x - 3)),
	deb_pretty_polynomial_roots('  7)', [2,3,2], ((x - 2)^2)*(x - 3)),
	deb_pretty_polynomial_roots('  8)', [2,3,2], ((x - 2)^2)*(x - 3)),
	deb_pretty_polynomial_roots('  9)', [3,2,2], ((x - 2)^2)*(x - 3)),

	write('* POLYNOMIAL ROOTS'), nl,

	deb_poly_roots('  1)', (x + 1)^2, [-1, -1]),
	deb_poly_roots('  2)', ((x + 1)^2)*(x - 3), [-1, -1, 3]),
	deb_poly_roots('  3)', ((x + 1)^2)*(x - 3)^2, [-1, -1, 3, 3]),
	deb_poly_roots('  4)', ((x + 1)^2)*(x - 3)*(x + 3), [-1, -1, -3, 3]),
	deb_poly_roots('  5)', (x + 1)*(x - 1)*(x - 3)*(x + 3), [-1, 1, -3, 3]),

	write('* POLYNOMIAL ROOTS -> PRETTY -> EVAL -> ROOTS'), nl,

	deb_poly_roots_eval_roots('  1)', [1]),
	deb_poly_roots_eval_roots('  2)', [1,1]),
	deb_poly_roots_eval_roots('  3)', [2,1]),
	deb_poly_roots_eval_roots('  4)', [2,1,3]),
	deb_poly_roots_eval_roots('  5)', [2,-1,3]),
	deb_poly_roots_eval_roots('  6)', [2,-1,-3]),
	deb_poly_roots_eval_roots('  7)', [-2,1]),
	deb_poly_roots_eval_roots('  8)', [-2,1,4]),

	write('* EXPANDED POLYNOMIAL EVALUATION'), nl,

	deb_exp_poly_eval('  1)',          3*x, x, 1, 3),
	deb_exp_poly_eval('  2)',            x, x, 1, 1),
	deb_exp_poly_eval('  3)',          x^2, x, 1, 1),
	deb_exp_poly_eval('  4)',      x^2 + x, x, 1, 2),
	deb_exp_poly_eval('  5)',      x^2 + 3, x, 1, 4),
	deb_exp_poly_eval('  6)',    4*x^2 + 3, x, -8, 259),
	deb_exp_poly_eval('  7)',    4*x^2 + 3, x, 7/8, 97/16),
	deb_exp_poly_eval('  8)', -4*x^3 + 3*x, x, 0, 0),
	deb_exp_poly_eval('  9)', -4*x^3 + 3*x, x, 9/5, -2241/125),
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

	write('* FALLING FACTORIALS'), nl,

	deb_falling_factorial('  1)',    n,		1,	n),
	deb_falling_factorial('  2)',    n - 1,	1,	n - 1),
	deb_falling_factorial('  3)',    n - 2,	1,	n - 2),
	deb_falling_factorial('  4)',    n,		2,	(1/2)*n*(n - 1)),
	deb_falling_factorial('  5)',    n - 1,	2,	(1/2)*(n - 1)*(n - 2)),
	deb_falling_factorial('  6)',    n - 2,	2,	(1/2)*(n - 2)*(n - 3)),
	deb_falling_factorial('  7)',    n,		3,	(1/6)*n*(n - 1)*(n - 2)),
	deb_falling_factorial('  8)',    n - 1,	3,	(1/6)*(n - 1)*(n - 2)*(n - 3)),
	deb_falling_factorial('  9)',    n - 2,	3,	(1/6)*(n - 2)*(n - 3)*(n - 4)),
	deb_falling_factorial(' 10)',    n - 7,	1,	n - 7),
	deb_falling_factorial(' 11)',    n - 7,	2,	(1/2)*(n - 7)*(n - 8)),
	deb_falling_factorial(' 12)',    n - 7,	3,	(1/6)*(n - 7)*(n - 8)*(n - 9)),
	deb_falling_factorial(' 13)',    n - 7,	4,	(1/24)*(n - 7)*(n - 8)*(n - 9)*(n - 10)),
	deb_falling_factorial(' 14)',  2*n - 7,	4,	(1/24)*(2*n - 7)*(2*n - 8)*(2*n - 9)*(2*n - 10)),
	deb_falling_factorial(' 15)',  3*n - 7,	4,	(1/24)*(3*n - 7)*(3*n - 8)*(3*n - 9)*(3*n - 10)),
	deb_falling_factorial(' 16)', -4*n + 7,	4,	(1/24)*(-4*n + 7)*(-4*n + 6)*(-4*n + 5)*(-4*n + 4)),

	nl, true.

/*
-----------------------------
DEBUG - POWER SUMS
*/

deb_power_sum_D(I, N, F, D):-
	expanded_polynomial_evaluation(N, n, F, RES), sum_from_1_to_n_to_D(N, D, RES), !, output_correct(I).
deb_power_sum_D(I, N, F, D):-
	expanded_polynomial_evaluation(N, n, F, RES), sum_from_1_to_n_to_D(N, D, R),
	write(I), write(' Sum from 1 to '), write(N), write(' = '), output_text(R, RES).

debug_power_sums:-
	F = power_sums,
	write('-- POWER SUMS --'), nl,
	write('* D = 0'), nl,

	call(F, 0, D0),
	deb_power_sum_D(' 0.1)', 1, D0, 0),
	deb_power_sum_D(' 0.2)', 5, D0, 0),
	deb_power_sum_D(' 0.3)', 10, D0, 0),
	deb_power_sum_D(' 0.4)', 20, D0, 0),
	deb_power_sum_D(' 0.5)', 30, D0, 0),
	deb_power_sum_D(' 0.6)', 50, D0, 0),
	deb_power_sum_D(' 0.7)', 75, D0, 0),
	deb_power_sum_D(' 0.8)', 100, D0, 0),

	write('* D = 1'), nl,

	call(F, 1, D1),
	deb_power_sum_D(' 1.1)', 1, D1, 1),
	deb_power_sum_D(' 1.2)', 5, D1, 1),
	deb_power_sum_D(' 1.3)', 10, D1, 1),
	deb_power_sum_D(' 1.4)', 20, D1, 1),
	deb_power_sum_D(' 1.5)', 30, D1, 1),
	deb_power_sum_D(' 1.6)', 50, D1, 1),
	deb_power_sum_D(' 1.7)', 75, D1, 1),
	deb_power_sum_D(' 1.8)', 100, D1, 1),

	write('* D = 2'), nl,

	call(F, 2, D2),
	deb_power_sum_D(' 2.1)', 1, D2, 2),
	deb_power_sum_D(' 2.2)', 5, D2, 2),
	deb_power_sum_D(' 2.3)', 10, D2, 2),
	deb_power_sum_D(' 2.4)', 20, D2, 2),
	deb_power_sum_D(' 2.5)', 30, D2, 2),
	deb_power_sum_D(' 2.6)', 50, D2, 2),
	deb_power_sum_D(' 2.7)', 75, D2, 2),
	deb_power_sum_D(' 2.8)', 100, D2, 2),

	write('* D = 3'), nl,

	call(F, 3, D3),
	deb_power_sum_D(' 3.1)', 1, D3, 3),
	deb_power_sum_D(' 3.2)', 5, D3, 3),
	deb_power_sum_D(' 3.3)', 10, D3, 3),
	deb_power_sum_D(' 3.4)', 20, D3, 3),
	deb_power_sum_D(' 3.5)', 30, D3, 3),
	deb_power_sum_D(' 3.6)', 50, D3, 3),
	deb_power_sum_D(' 3.7)', 75, D3, 3),
	deb_power_sum_D(' 3.8)', 100, D3, 3),

	write('* D = 5'), nl,

	call(F, 5, D5),
	deb_power_sum_D(' 5.1)', 1, D5, 5),
	deb_power_sum_D(' 5.2)', 5, D5, 5),
	deb_power_sum_D(' 5.3)', 10, D5, 5),
	deb_power_sum_D(' 5.4)', 20, D5, 5),
	deb_power_sum_D(' 5.5)', 30, D5, 5),
	deb_power_sum_D(' 5.6)', 50, D5, 5),
	deb_power_sum_D(' 5.7)', 75, D5, 5),
	deb_power_sum_D(' 5.8)', 100, D5, 5),

	write('* D = 7'), nl,

	call(F, 7, D7),
	deb_power_sum_D(' 7.1)', 1, D7, 7),
	deb_power_sum_D(' 7.2)', 5, D7, 7),
	deb_power_sum_D(' 7.3)', 10, D7, 7),
	deb_power_sum_D(' 7.4)', 20, D7, 7),
	deb_power_sum_D(' 7.5)', 30, D7, 7),
	deb_power_sum_D(' 7.6)', 50, D7, 7),
	deb_power_sum_D(' 7.7)', 75, D7, 7),
	deb_power_sum_D(' 7.8)', 100, D7, 7),

	write('* D = 10'), nl,

	call(F, 10, D10),
	deb_power_sum_D(' 10.1)', 1, D10, 10),
	deb_power_sum_D(' 10.2)', 5, D10, 10),
	deb_power_sum_D(' 10.3)', 10, D10, 10),
	deb_power_sum_D(' 10.4)', 20, D10, 10),
	deb_power_sum_D(' 10.5)', 30, D10, 10),
	deb_power_sum_D(' 10.6)', 50, D10, 10),
	deb_power_sum_D(' 10.7)', 75, D10, 10),
	deb_power_sum_D(' 10.8)', 100, D10, 10),

	write('* D = 15'), nl,

	call(F, 15, D15),
	deb_power_sum_D(' 15.1)', 1, D15, 15),
	deb_power_sum_D(' 15.2)', 5, D15, 15),
	deb_power_sum_D(' 15.3)', 10, D15, 15),
	deb_power_sum_D(' 15.4)', 20, D15, 15),
	deb_power_sum_D(' 15.5)', 30, D15, 15),
	deb_power_sum_D(' 15.6)', 50, D15, 15),
	deb_power_sum_D(' 15.7)', 75, D15, 15),
	deb_power_sum_D(' 15.8)', 100, D15, 15),

	write('* D = 20'), nl,

	call(F, 20, D20),
	deb_power_sum_D(' 20.1)', 1, D20, 20),
	deb_power_sum_D(' 20.2)', 5, D20, 20),
	deb_power_sum_D(' 20.3)', 10, D20, 20),
	deb_power_sum_D(' 20.4)', 20, D20, 20),
	deb_power_sum_D(' 20.5)', 30, D20, 20),
	deb_power_sum_D(' 20.6)', 50, D20, 20),
	deb_power_sum_D(' 20.7)', 75, D20, 20),
	deb_power_sum_D(' 20.8)', 100, D20, 20),

	nl, true.
