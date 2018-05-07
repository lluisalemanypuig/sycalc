:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(common).

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

deb_poly_eval(I, E, RES):- polynomial_expression_evaluation(E, R), polynomial_eq(R, RES), !, output_correct(I).
deb_poly_eval(I, E, RES):-
	polynomial_expression_evaluation(E, R),
	write(I), write(' '), write(E), write(' = '), output_text(R, RES).

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
	polynomial_expression_evaluation(P, E), integer_roots_polynomial(E, R),
	msort(R, SR), msort(RES, SRES), SR == SRES, !, output_correct(I).
deb_poly_roots(I, P, RES):-
	polynomial_expression_evaluation(P, E), integer_roots_polynomial(E, R),
	write(I), write(' '), write(P), write(' -> '), output_text(R, RES).

deb_poly_roots_eval_roots(I, R):-
	pretty_polynomial_roots(R, P), polynomial_expression_evaluation(P, Q),
	integer_roots_polynomial(Q, RES), sort(R, RS), sort(RES, RESS),
	RS == RESS, !, output_correct(I).
deb_poly_roots_eval_roots(I, R):-
	write(I), write(' '),
	pretty_polynomial_roots(R, P),
	polynomial_expression_evaluation(P, Q),
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
	polynomial_expression_evaluation(choose(P, F), R), polynomial_expression_evaluation(RES, R), !, output_correct(I).
deb_falling_factorial(I, P, F, RES):-
	polynomial_expression_evaluation(choose(P, F), R),
	write(I), write(' choose('), write(P), write(','), write(F), write(')= '), write(R),
	polynomial_expression_evaluation(RES, CORRECT_OUTPUT), output_text(R, CORRECT_OUTPUT).

deb_mono_eval(I, M,V, E, RES):-
	monomial_symb_evaluation(V, E, M, RES), !, output_correct(I).
deb_mono_eval(I, M,V, E, RES):-
	monomial_symb_evaluation(V, E, M, N),
	write(I), write(' '),
	write(M), write( '(-> '), write(E), write(' @ '), write(V), write(' )= '),
	output_text(N, RES).

deb_mono_summation(CASE, VAR, I,F, M, RES):-
	polynomial_expression_evaluation(RES, RESE), monomial_summation(VAR, I,F, M, RESE), !, output_correct(CASE).
deb_mono_summation(CASE, VAR, I,F, M, RES):-
	monomial_summation(VAR, I,F, M, S),
	write(CASE), write(' '),
	write('sum_{'), write(VAR), write('='), write(I), write('}^{'), write(F), write('} ('), write(M), write(')= '),
	output_text(S, RES).

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

	write('* MONOMIAL SUMMATION'), nl,

	deb_mono_summation('  1)', j,0,10,  i, 11*i),
	deb_mono_summation('  2)', j,5,10,  i, 6*i),
	deb_mono_summation('  3)', j,10,10, i, i),
	deb_mono_summation('  4)', j,0,10,  i^2, 11*i^2),
	deb_mono_summation('  5)', j,5,10,  i^2, 6*i^2),
	deb_mono_summation('  6)', j,10,10, i^2, i^2),
	deb_mono_summation('  7)', j,0,10,  i*k, 11*i*k),
	deb_mono_summation('  8)', j,5,10,  i*k, 6*i*k),
	deb_mono_summation('  9)', j,10,10, i*k^2, i*k^2),
	deb_mono_summation(' 10)', j,0,10,  i^2*k, 11*i^2*k),
	deb_mono_summation(' 11)', j,5,10,  i^2*k, 6*i^2*k),
	deb_mono_summation(' 12)', j,10,10, i^2*k^2, i^2*k^2),

	deb_mono_summation(' 13)', j,10,0,  i, 0),
	deb_mono_summation(' 14)', j,10,5,  i, 0),
	deb_mono_summation(' 15)', j,10,10, i, i),
	deb_mono_summation(' 16)', j,10,0,  i^2, 0),
	deb_mono_summation(' 17)', j,10,5,  i^2, 0),
	deb_mono_summation(' 18)', j,10,10, i^2, i^2),
	deb_mono_summation(' 19)', j,10,0,  i*k, 0),
	deb_mono_summation(' 20)', j,10,5,  i*k, 0),
	deb_mono_summation(' 21)', j,10,10, i*k^2, i*k^2),
	deb_mono_summation(' 22)', j,10,0,  i^2*k, 0),
	deb_mono_summation(' 23)', j,10,5,  i^2*k, 0),
	deb_mono_summation(' 24)', j,10,10, i^2*k^2, i^2*k^2),

	deb_mono_summation(' 25)', j,0,y, i, (y+1)*i),
	deb_mono_summation(' 26)', j,5,y, i, (y-4)*i),
	deb_mono_summation(' 27)', j,10,y, i, (y-9)*i),
	deb_mono_summation(' 28)', j,0,y, i^2, (y+1)*i^2),
	deb_mono_summation(' 29)', j,5,y, i^2, (y-4)*i^2),
	deb_mono_summation(' 30)', j,10,y, i^2, (y-9)*i^2),
	deb_mono_summation(' 31)', j,0,y, i*k, (y+1)*i*k),
	deb_mono_summation(' 32)', j,5,y, i*k, (y-4)*i*k),
	deb_mono_summation(' 33)', j,10,y, i*k^2, (y-9)*i*k^2),
	deb_mono_summation(' 34)', j,0,y, i^2*k, (y+1)*i^2*k),
	deb_mono_summation(' 35)', j,5,y, i^2*k, (y-4)*i^2*k),
	deb_mono_summation(' 36)', j,10,y, i^2*k^2, (y-9)*i^2*k^2),

	deb_mono_summation(' 37)', j,x,0,  i, (0-x+1)*i),
	deb_mono_summation(' 38)', j,x,5,  i, (5-x+1)*i),
	deb_mono_summation(' 39)', j,x,10, i, (10-x+1)*i),
	deb_mono_summation(' 40)', j,x,0,  i^2, (0-x+1)*i^2),
	deb_mono_summation(' 41)', j,x,5,  i^2, (5-x+1)*i^2),
	deb_mono_summation(' 42)', j,x,10, i^2, (10-x+1)*i^2),
	deb_mono_summation(' 43)', j,x,0,  i*k, (0-x+1)*i*k),
	deb_mono_summation(' 44)', j,x,5,  i*k, (5-x+1)*i*k),
	deb_mono_summation(' 45)', j,x,10, i*k^2, (10-x+1)*i*k^2),
	deb_mono_summation(' 46)', j,x,0,  i^2*k, (0-x+1)*i^2*k),
	deb_mono_summation(' 47)', j,x,5,  i^2*k, (5-x+1)*i^2*k),
	deb_mono_summation(' 48)', j,x,10, i^2*k^2, (10-x+1)*i^2*k^2),

	deb_mono_summation(' 49)', j,x,y,  i, (y-x+1)*i),
	deb_mono_summation(' 50)', j,x,y,  i*k, (y-x+1)*i*k),
	deb_mono_summation(' 51)', j,x,y,  i^2, (y-x+1)*i^2),
	deb_mono_summation(' 52)', j,x,y,  i^2*k, (y-x+1)*i^2*k),

	deb_mono_summation(' 53)', i,0,10,  i, 55),
	deb_mono_summation(' 54)', i,5,10,  i, 45),
	deb_mono_summation(' 55)', i,10,10, i, 10),
	deb_mono_summation(' 56)', i,0,10,  i^2, 385),
	deb_mono_summation(' 57)', i,5,10,  i^2, 355),
	deb_mono_summation(' 58)', i,10,10, i^2, 100),
	deb_mono_summation(' 59)', i,0,10,  i*k, 55*k),
	deb_mono_summation(' 60)', i,5,10,  i*k, 45*k),
	deb_mono_summation(' 61)', i,10,10, i*k^2, 10*k^2),
	deb_mono_summation(' 62)', i,0,10,  i^2*k, 385*k),
	deb_mono_summation(' 63)', i,5,10,  i^2*k, 355*k),
	deb_mono_summation(' 64)', i,10,10, i^2*k^2, 100*k^2),

	deb_mono_summation(' 65)', i,10,0,  i, 0),
	deb_mono_summation(' 66)', i,10,5,  i, 0),
	deb_mono_summation(' 67)', i,10,10, i, 10),
	deb_mono_summation(' 68)', i,10,0,  i^2, 0),
	deb_mono_summation(' 69)', i,10,5,  i^2, 0),
	deb_mono_summation(' 70)', i,10,10, i^2, 100),
	deb_mono_summation(' 71)', i,10,0,  i*k, 0),
	deb_mono_summation(' 72)', i,10,5,  i*k, 0),
	deb_mono_summation(' 73)', i,10,10, i*k^2, 10*k^2),
	deb_mono_summation(' 74)', i,10,0,  i^2*k, 0),
	deb_mono_summation(' 75)', i,10,5,  i^2*k, 0),
	deb_mono_summation(' 76)', i,10,10, i^2*k^2, 100*k^2),

	deb_mono_summation(' 77)', i,0,y, i, 1/2*y*(y+1)),
	deb_mono_summation(' 78)', i,5,y, i, 1/2*y*(y+1) - 1/2*4*(4+1)),
	deb_mono_summation(' 79)', i,10,y, i, 1/2*y*(y+1) - 1/2*9*(9+1)),
	deb_mono_summation(' 80)', i,0,y, i^2, 1/6*y*(y+1)*(2*y+1)),
	deb_mono_summation(' 81)', i,5,y, i^2, 1/6*y*(y+1)*(2*y+1) - 1/6*4*(4+1)*(2*4+1)),
	deb_mono_summation(' 82)', i,10,y, i^2, 1/6*y*(y+1)*(2*y+1) - 1/6*9*(9+1)*(2*9+1)),
	deb_mono_summation(' 83)', i,0,y, i*k, k*(1/2*y*(y+1))),
	deb_mono_summation(' 82)', i,5,y, i*k, k*(1/2*y*(y+1) - 1/2*4*(4+1))),
	deb_mono_summation(' 83)', i,10,y, i*k^2, k^2*(1/2*y*(y+1) - 1/2*9*(9+1))),
	deb_mono_summation(' 84)', i,0,y, i^2*k, k*(1/6*y*(y+1)*(2*y+1))),
	deb_mono_summation(' 85)', i,5,y, i^2*k, k*(1/6*y*(y+1)*(2*y+1) - 1/6*4*(4+1)*(2*4+1))),
	deb_mono_summation(' 86)', i,10,y, i^2*k^2, k^2*(1/6*y*(y+1)*(2*y+1) - 1/6*9*(9+1)*(2*9+1))),

	deb_mono_summation(' 87)', i,x,0,  i, 1/2*0*(0+1) - 1/2*x*(x-1)),
	deb_mono_summation(' 88)', i,x,5,  i, 1/2*5*(5+1) - 1/2*x*(x-1)),
	deb_mono_summation(' 89)', i,x,10, i, 1/2*10*(10+1) - 1/2*x*(x-1)),
	deb_mono_summation(' 90)', i,x,0,  i^2, 1/6*0*(0+1)*(2*0+1) - 1/6*x*(x-1)*(2*x-1)),
	deb_mono_summation(' 91)', i,x,5,  i^2, 1/6*5*(5+1)*(2*5+1) - 1/6*x*(x-1)*(2*x-1)),
	deb_mono_summation(' 92)', i,x,10, i^2, 1/6*10*(10+1)*(2*10+1) - 1/6*x*(x-1)*(2*x-1)),
	deb_mono_summation(' 93)', i,x,0,  i*k, k*(1/2*0*(0+1) - 1/2*x*(x-1))),
	deb_mono_summation(' 94)', i,x,5,  i*k, k*(1/2*5*(5+1) - 1/2*x*(x-1))),
	deb_mono_summation(' 95)', i,x,10, i*k^2, k^2*(1/2*10*(10+1) - 1/2*x*(x-1))),
	deb_mono_summation(' 96)', i,x,0,  i^2*k, k*(1/6*0*(0+1)*(2*0+1) - 1/6*x*(x-1)*(2*x-1))),
	deb_mono_summation(' 97)', i,x,5,  i^2*k, k*(1/6*5*(5+1)*(2*5+1) - 1/6*x*(x-1)*(2*x-1))),
	deb_mono_summation(' 98)', i,x,10, i^2*k^2, k^2*(1/6*10*(10+1)*(2*10+1) - 1/6*x*(x-1)*(2*x-1))),

	deb_mono_summation(' 99)', i,x,y,  i, (1/2)*( (y-x+1)*(x+y) )),
	deb_mono_summation('100)', i,x,y,  i*k, k*(1/2)*( (y-x+1)*(x+y) )),
	deb_mono_summation('101)', i,x,y,  i^2, (1/6)*(2*(y^3-x^3) + 3*(y^2+x^2) + y-x)),
	deb_mono_summation('102)', i,x,y,  i^2*k, k*(1/6)*(2*(y^3-x^3) + 3*(y^2+x^2) + y-x)),

	nl, true.