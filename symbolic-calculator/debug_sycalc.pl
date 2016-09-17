:-ensure_loaded(arithmetic_evaluation).
:-ensure_loaded(polynomial_evaluation).
:-ensure_loaded(numerical_algorithms).
:-ensure_loaded(monomial_evaluation).
:-ensure_loaded(polynomials).
:-ensure_loaded(monomials).
:-ensure_loaded(numbers).
:-ensure_loaded(lists).

debug:-
	debug_numeric,
	debug_monomials,
	debug_polynomials.

main:- debug, halt.
main:- nl, write('ERROR'), nl, halt.

/*
-----------------------------
DEBUG - ARITHMETIC EVALUATION
*/

deb_red_frac(F, RES):- red_frac(F, R), write(F), write(' = '), write(R), write(' | correct? '), R == RES, !, write('Yes'), nl.
deb_red_frac(_, RES):- write('No - Expected to see '), write(RES), nl, false.

sum_deb(A, B, RES):- eval_sum(A, B, R), write(A), write(' + '), write(B), write(' = '), write(R), write(' | correct? '), RES == R, !, write('Yes'), nl.
sum_deb(_, _, RES):- write('No - Expected to see '), write(RES), nl, false.

sub_deb(A, B, RES):- eval_sub(A, B, R), write(A), write(' - '), write(B), write(' = '), write(R), write(' | correct? '), RES == R, !, write('Yes'), nl.
sub_deb(_, _, RES):- write('No - Expected to see '), write(RES), nl, false.

prod_deb(A, B, RES):- eval_prod(A, B, R), write(A), write('*'), write(B), write(' = '), write(R), write(' | correct? '), RES == R, !, write('Yes'), nl.
prod_deb(_, _, RES):- write('No - Expected to see '), write(RES), nl, false.

power_deb(A, B, RES):- eval_pow(A, B, R), write(A), write('^'), write(B), write(' = '), write(R), write(' | correct? '), RES == R, !, write('Yes'), nl.
power_deb(_, _, RES):- write('No - Expected to see '), write(RES), nl, false.

arithm_deb(E, RES):- arithmetic_eval(E, R), write(E), write(' = '), write(R), write(' | correct? '), RES == R, !, write('Yes'), nl.
arithm_deb(_, RES):- write('No - Expected to see '), write(RES), nl, false.

debug_numeric:-
	nl, write('-- NUMERICAL DEBUG --'), nl,
	write('- REDUCED FRACTIONS -'), nl, nl,

	write(' 1) '), deb_red_frac(0/1, 0),
	write(' 2) '), deb_red_frac(0/5, 0),
	write(' 3) '), deb_red_frac(1/2, 1/2),
	write(' 4) '), deb_red_frac(4/2, 2),
	write(' 5) '), deb_red_frac(5/2, 5/2),
	write(' 6) '), deb_red_frac(6/2, 3),
	write(' 7) '), deb_red_frac(6/4, 3/2),
	write(' 8) '), deb_red_frac(6/8, 3/4),
	write(' 9) '), deb_red_frac(6/10, 3/5),
	write('10) '), deb_red_frac(-0/1, 0),
	write('11) '), deb_red_frac(-0/5, 0),
	write('12) '), deb_red_frac(-1/2, -1/2),
	write('13) '), deb_red_frac(-4/2, -2),
	write('14) '), deb_red_frac(-5/2, -5/2),
	write('15) '), deb_red_frac(-6/2, -3),
	write('16) '), deb_red_frac(-6/4, -3/2),
	write('17) '), deb_red_frac(-6/8, -3/4),
	write('18) '), deb_red_frac(-6/10, -3/5),
	
	nl, write('- SUMS -'), nl, nl,

	write(' 1) '), sum_deb(0, 0, 0),
	write(' 2) '), sum_deb(1, 0, 1),
	write(' 3) '), sum_deb(4, 0, 4),
	write(' 4) '), sum_deb(0, 1, 1),
	write(' 5) '), sum_deb(0, 5, 5),
	write(' 6) '), sum_deb(1/2, 1/2, 1),
	write(' 7) '), sum_deb(1/2, 2/2, 3/2),
	write(' 8) '), sum_deb(1/2, 1, 3/2),
	write(' 9) '), sum_deb(1, 1/2, 3/2),
	write('10) '), sum_deb(2/2, 2/2, 2),
	write('11) '), sum_deb(2/2, 3/2, 5/2),
	write('12) '), sum_deb(3/2, 1, 5/2),
	
	nl, write('- SUBS -'), nl, nl,

	write(' 1) '), sub_deb(0, 0, 0),
	write(' 2) '), sub_deb(1, 0, 1),
	write(' 3) '), sub_deb(4, 0, 4),
	write(' 4) '), sub_deb(0, 1, -1),
	write(' 5) '), sub_deb(0, 5, -5),
	write(' 6) '), sub_deb(1/2, 1/2, 0),
	write(' 7) '), sub_deb(1/2, 2/2, -1/2),
	write(' 8) '), sub_deb(1/2, 1, -1/2),
	write(' 9) '), sub_deb(1, 1/2, 1/2),
	write('10) '), sub_deb(2/2, 2/2, 0),
	write('11) '), sub_deb(2/2, 3/2, -1/2),
	write('12) '), sub_deb(3/2, 1, 1/2),
	
	nl, write('- PRODS -'), nl, nl,

	write(' 1) '), prod_deb(0, 0, 0),
	write(' 2) '), prod_deb(1, 0, 0),
	write(' 3) '), prod_deb(4, 0, 0),
	write(' 4) '), prod_deb(0, 1, 0),
	write(' 5) '), prod_deb(0, 5, 0),
	write(' 6) '), prod_deb(1/2, 1/2, 1/4),
	write(' 7) '), prod_deb(1/2, 2/2, 1/2),
	write(' 8) '), prod_deb(1/2, 1, 1/2),
	write(' 9) '), prod_deb(1, 1/2, 1/2),
	write('10) '), prod_deb(2/2, 2/2, 1),
	write('11) '), prod_deb(2/2, 3/2, 3/2),
	write('12) '), prod_deb(3/2, 1, 3/2),
	
	nl, write('- POWERS -'), nl, nl,

	write(' 1) '), power_deb(0, 0, 1),
	write(' 2) '), power_deb(1, 0, 1),
	write(' 3) '), power_deb(4, 0, 1),
	write(' 4) '), power_deb(0, 1, 0),
	write(' 5) '), power_deb(0, 5, 0),
	write(' 6) '), power_deb(1/2, 2, 1/4),
	write(' 7) '), power_deb(1/2, 1, 1/2),
	write(' 8) '), power_deb(2, 3, 8),
	write(' 9) '), power_deb(2/2, 2, 1),
	write('10) '), power_deb(2/2, 3, 1),
	write('11) '), power_deb(3/2, 1, 3/2),
	write('12) '), power_deb(3/2, 2, 9/4),
	
	nl, write('- ARITHMETIC EXPRESSIONS -'), nl, nl,
	write(' 1) '), arithm_deb(3 + 3, 6),
	write(' 2) '), arithm_deb(3 - 4, -1),
	write(' 3) '), arithm_deb(3^4, 81),
	write(' 4) '), arithm_deb(2^2^2^2, 256),
	write(' 5) '), arithm_deb(2^2^2^2^2, 65536),
	write(' 6) '), arithm_deb(2^(1 + 1)^2, 16),
	write(' 7) '), arithm_deb(2^2^(2 + 1 - 1)^2^2, 65536),
	write(' 8) '), arithm_deb((1/2)^2 + 3/4, 1),
	write(' 9) '), arithm_deb((1/2)^0, 1),
	write('10) '), arithm_deb((1/2)^0 + 3/4, 7/4),
	write('11) '), arithm_deb(1 - (1 + 1), -1),
	write('12) '), arithm_deb(1/2, 1/2),
	true.

/*
-----------------------------
DEBUG - MONOMIAL EVALUATION
*/

deb_red_mon(M, RES):- write(M), write(' = '), red_monomial(M, R), write(R), write(' | correct? '), R == RES, write('Yes'), nl, !.
deb_red_mon(_, RES):- write('No - Expected to see '), write(RES), nl, false.

deb_mon_sum(M1, M2, RES):- write(M1), write(' + '), write(M2), write(' = '), mon_sum(M1, M2, S), write(S), write(' | correct? '), S == RES, write('Yes'), nl, !.
deb_mon_sum( _,  _, RES):- write('No - Expected to see '), write(RES), nl, false.

deb_mon_sub(M1, M2, RES):- write(M1), write(' - '), write(M2), write(' = '), mon_sub(M1, M2, S), write(S), write(' | correct? '), S == RES, write('Yes'), nl, !.
deb_mon_sub( _,  _, RES):- write('No - Expected to see '), write(RES), nl, false.

deb_mon_prod(M1, M2, RES):- write(M1), write('*'), write(M2), write(' = '), mon_prod(M1, M2, S), write(S), write(' | correct? '), S == RES, write('Yes'), nl, !.
deb_mon_prod( _,  _, RES):- write('No - Expected to see '), write(RES), nl, false.

debug_monomials:-
	nl, write('-- MONOMIALS DEBUG --'), nl,
	write('- MONOMIAL REDUCTION -'), nl, nl,

	write(' 1) '), deb_red_mon(0*x^0, 0),
	write(' 2) '), deb_red_mon(0*x^1, 0),
	write(' 3) '), deb_red_mon(0*x^5, 0),
	
	write(' 4) '), deb_red_mon(1*x^0, 1),
	write(' 5) '), deb_red_mon(1*x^1, x),
	write(' 6) '), deb_red_mon(1*x^5, x^5),
	
	write(' 7) '), deb_red_mon(3*x^0, 3),
	write(' 8) '), deb_red_mon(3*x^1, 3*x),
	write(' 9) '), deb_red_mon(3*x^6, 3*x^6),

	write('10) '), deb_red_mon((0/1)*x^(0/1), 0),
	write('11) '), deb_red_mon((0/8)*x^(4/4), 0),
	write('12) '), deb_red_mon((0/3)*x^(20/4), 0),

	write('13) '), deb_red_mon((7/7)*x^(0/9), 1),
	write('14) '), deb_red_mon((6/6)*x^(2/2), x),
	write('15) '), deb_red_mon((1/1)*x^(5/1), x^5),

	write('16) '), deb_red_mon((27/9)*x^(0/1), 3),
	write('17) '), deb_red_mon((18/6)*x^(8/8), 3*x),
	write('18) '), deb_red_mon((15/5)*x^(36/6), 3*x^6),

	write('19) '), deb_red_mon(-0*x^0, 0),
	write('20) '), deb_red_mon(-(0*x^0), 0),
	write('21) '), deb_red_mon(-0*x^1, 0),
	write('22) '), deb_red_mon(-(0*x^1), 0),
	write('23) '), deb_red_mon(-0*x^5, 0),
	write('24) '), deb_red_mon(-(0*x^5), 0),
	
	write('25) '), deb_red_mon(-1*x^0, -1),
	write('26) '), deb_red_mon(-(1*x^0), -1),
	write('27) '), deb_red_mon(-1*x^1, -x),
	write('28) '), deb_red_mon(-(1*x^1), -x),
	write('29) '), deb_red_mon(-1*x^5, -x^5),
	write('30) '), deb_red_mon(-(1*x^5), -x^5),
	
	write('31) '), deb_red_mon(-3*x^0, -3),
	write('32) '), deb_red_mon(-(3*x^0), -3),
	write('33) '), deb_red_mon(-3*x^1, -3*x),
	write('34) '), deb_red_mon(-(3*x^1), -3*x),
	write('35) '), deb_red_mon(-3*x^6, -3*x^6),
	write('36) '), deb_red_mon(-(3*x^6), -3*x^6),
	
	write('37) '), deb_red_mon(-((0/1)*x^(0/1)), 0),
	write('38) '), deb_red_mon((-0/1)*x^(0/1), 0),
	write('39) '), deb_red_mon((0/1)*x^(-0/1), 0),
	write('40) '), deb_red_mon((0/1)*x^(-(0/1)), 0),
	write('41) '), deb_red_mon(-((0/8)*x^(4/4)), 0),
	write('42) '), deb_red_mon((-0/8)*x^(-4/4), 0),
	write('43) '), deb_red_mon(-((0/3)*x^(20/4)), 0),
	write('44) '), deb_red_mon((-0/3)*x^(-20/4), 0),

	write('45) '), deb_red_mon(-((7/7)*x^(0/9)), -1),
	write('46) '), deb_red_mon((-7/7)*x^(0/9), -1),
	write('47) '), deb_red_mon((7/7)*x^(-0/9), 1),

	write('48) '), deb_red_mon(-((6/6)*x^(2/2)), -x),
	write('49) '), deb_red_mon((-6/6)*x^(2/2), -x),
	write('50) '), deb_red_mon((6/6)*x^(-2/2), x^(-1)),

	write('51) '), deb_red_mon(-((1/1)*x^(5/1)), -x^5),
	write('52) '), deb_red_mon((-1/1)*x^(5/1), -x^5),
	write('53) '), deb_red_mon((1/1)*x^(-5/1), x^(-5)),
	
	write('54) '), deb_red_mon(-((27/9)*x^(0/1)), -3),
	write('56) '), deb_red_mon((-27/9)*x^(0/1), -3),
	write('57) '), deb_red_mon((27/9)*x^(-0/1), 3),

	write('58) '), deb_red_mon(-((18/6)*x^(8/8)), -3*x),
	write('59) '), deb_red_mon((-18/6)*x^(8/8), -3*x),
	write('60) '), deb_red_mon((18/6)*x^(-8/7), 3*x^(-8/7)),

	write('61) '), deb_red_mon(-((15/5)*x^(36/8)), -3*x^(9/2)),
	write('62) '), deb_red_mon((-15/5)*x^(36/8), -3*x^(9/2)),
	write('63) '), deb_red_mon((15/5)*x^(-36/8), 3*x^(-9/2)),

	write('64) '), deb_red_mon(-(1/2*x), -1/2*x),
	write('65) '), deb_red_mon(-1/2*x, -1/2*x),
	write('66) '), deb_red_mon((-1/2)*x, -1/2*x),
	
	nl, write('- MONOMIAL SUM -'), nl, nl,

	write(' 1) '), deb_mon_sum(3*x, 2*x, 5*x),
	write(' 2) '), deb_mon_sum(3*x, 2*x^0, 3*x + 2),
	write(' 3) '), deb_mon_sum(3*x, 2*x^1, 5*x),
	write(' 4) '), deb_mon_sum(3*x^0, 2*x, 3 + 2*x),
	write(' 5) '), deb_mon_sum(3*x^1, 2*x, 5*x),
	write(' 6) '), deb_mon_sum(3*x^2, 2*x, 3*x^2 + 2*x),
	write(' 7) '), deb_mon_sum(3*x^2, 2*x^2, 5*x^2),
	write(' 8) '), deb_mon_sum(3*x^0, 2*x^0, 5),
	write(' 9) '), deb_mon_sum(3*x^0, 0*x^2, 3),
	write('10) '), deb_mon_sum(0*x^1, 0*x^2, 0),
	write('11) '), deb_mon_sum(0, 0*x^2, 0),
	write('12) '), deb_mon_sum(0, 0, 0),
	
	nl, write('- MONOMIAL SUB -'), nl, nl,

	write(' 1) '), deb_mon_sub(0, x, -x),
	write(' 2) '), deb_mon_sub(3*x, 2*x, x),
	write(' 3) '), deb_mon_sub(3*x, 2*x^0, 3*x - 2),
	write(' 4) '), deb_mon_sub(3*x, 2*x^1, x),
	write(' 5) '), deb_mon_sub(3*x^0, 2*x, 3 - 2*x),
	write(' 6) '), deb_mon_sub(3*x^1, 2*x, x),
	write(' 7) '), deb_mon_sub(3*x^2, 2*x, 3*x^2 - 2*x),
	write(' 8) '), deb_mon_sub(3*x^2, 2*x^2, x^2),
	write(' 9) '), deb_mon_sub(3*x^0, 2*x^0, 1),
	write('10) '), deb_mon_sub(3*x^0, 0*x^2, 3),
	write('11) '), deb_mon_sub(0*x^1, 0*x^2, 0),
	write('12) '), deb_mon_sub(0, 0*x^2, 0),
	write('13) '), deb_mon_sub(0, 0, 0),
	
	nl, write('- MONOMIAL PROD -'), nl, nl,

	write(' 1) '), deb_mon_prod(3*x, 2*x, 6*x^2),
	write(' 2) '), deb_mon_prod(3*x, 2*x^0, 6*x),
	write(' 3) '), deb_mon_prod(3*x, 2*x^1, 6*x^2),
	write(' 4) '), deb_mon_prod(3*x^0, 2*x, 6*x),
	write(' 5) '), deb_mon_prod(3*x^1, 2*x, 6*x^2),
	write(' 6) '), deb_mon_prod(3*x^2, 2*x, 6*x^3),
	write(' 7) '), deb_mon_prod(3*x^2, 2*x^2, 6*x^4),
	write(' 8) '), deb_mon_prod(3*x^0, 2*x^0, 6),
	write(' 9) '), deb_mon_prod(3*x^0, 0*x^2, 0),
	write('10) '), deb_mon_prod(0*x^1, 0*x^2, 0),
	write('11) '), deb_mon_prod(0, 0*x^2, 0),
	write('12) '), deb_mon_prod(0, 0, 0),

	true.

/*
-----------------------------
DEBUG - POLYNOMIAL EVALUATION
*/

deb_poly_sum(P, RES):- write(P), write(' = '), polynomial_sum(P, R), write(R), write(' | correct? '), polynomial_eq(R, RES), write('Yes'), !, nl.
deb_poly_sum(_, RES):- write('No - Expected to see '), write(RES), nl, false.

deb_poly_prod(P1, P2, RES):- write('('), write(P1), write(')*('), write(P2), write(') = '), polynomial_prod(P1, P2, R), write(R), write(' | correct? '), polynomial_eq(R, RES), write('Yes'), !, nl.
deb_poly_prod(_,   _, RES):- write('No - Expected to see '), write(RES), nl, false.

deb_poly_pow(P, N, RES):- write('('), write(P), write(')^'), write(N), write(' = '), polynomial_power(P, N, R), write(R), write(' | correct? '), polynomial_eq(R, RES), write('Yes'), !, nl.
deb_poly_pow(_, _, RES):- write('No - Expected to see '), write(RES), nl, false.

deb_poly_eval(E, RES):- write(E), write(' = '), polynomial_evaluation(E, R), write(R), write(' | correct? '), polynomial_eq(R, RES), write('Yes'), !, nl.
deb_poly_eval(_, RES):- write('No - Expected to see '), write(RES), nl, false.

debug_polynomials:-
	nl, write('-- POLYNOMIALS DEBUG --'), nl,

	nl, write('- POLYNOMIAL REDUCTION -'), nl, nl,

	write(' 1) '), deb_poly_sum(0*x^0, 0),
	write(' 2) '), deb_poly_sum(0 + x + 0, x),
	write(' 3) '), deb_poly_sum(0 + x^2 + x^(1 + 1), 2*x^2),
	write(' 4) '), deb_poly_sum(x^2 - 2*x^(3 - 1), -x^2),
	write(' 5) '), deb_poly_sum(x^2 - 2*x^(3 - 1), -x^2),
	write(' 6) '), deb_poly_sum((1/2)*x - (1/2)*x, 0),
	write(' 7) '), deb_poly_sum((1/2)*x + x^2 - (1/2)*x, x^2),
	write(' 8) '), deb_poly_sum((1/2)*x - (3/2)*x, -x),
	write(' 9) '), deb_poly_sum(0 + (1/2)*x + (3/2)*x, 2*x),
	write(' 10) '), deb_poly_sum(0 + (1/2)*x - 0 + (3/2)*x, 2*x),
	write(' 11) '), deb_poly_sum(x^2 + (1/2)*x - 0 + (3/2)*x, 2*x + x^2),
	write(' 12) '), deb_poly_sum(x - x^2 + x^3 - x^4 + x^5, x - x^2 + x^3 - x^4 + x^5),
	write(' 13) '), deb_poly_sum((1/3)*x^4 + (1/2)*x^3 + (1/6)*x^2 - (1/6)*x^3 + (1/4)*x^2 - (1/12)*x - (1/12)*x^2 + (1/12)*x - x^3, (1/3)*x^2 - (2/3)*x^3 + (1/3)*x^4),

	nl, write('- POLYNOMIAL PRODUCT -'), nl, nl,

	write(' 1) '), deb_poly_prod(x, x, x^2),
	write(' 2) '), deb_poly_prod(x^2, x, x^3),
	write(' 3) '), deb_poly_prod(x + 1, x, x^2 + x),
	write(' 4) '), deb_poly_prod(x, x + 1, x^2 + x),
	write(' 5) '), deb_poly_prod(x + 1, x + 1, x^2 + 2*x + 1),
	write(' 5) '), deb_poly_prod(x^2 + 1, x + 1, x^3 + x^2 + x + 1),
	write(' 6) '), deb_poly_prod(x^2 + 2, 6*x + 1, 6*x^3 + x^2 + 12*x + 2),
	write(' 7) '), deb_poly_prod(x^2 + 2*x^2, -6*x - 1, -18*x^3 - 3*x^2),
	write(' 8) '), deb_poly_prod(x^2 + 0, -6*x - 1, -6*x^3 - x^2),
	write(' 9) '), deb_poly_prod(-1 + x^2 + 1, -6*x + 1, -6*x^3 + x^2),
	write('10) '), deb_poly_prod(-1 + x^2 + 1, -1 + x^2 + 1, x^4),
	write('11) '), deb_poly_prod(2*x^2 - 3*x^3 + 30, 2*x^2 - 3*x^3 + 30, 9*x^6 - 12*x^5 + 4*x^4 - 180*x^3 + 120*x^2 + 900),

	nl, write('- POLYNOMIAL POWER -'), nl, nl,
	
	write(' 1) '), deb_poly_pow(x, 0, 1),
	write(' 2) '), deb_poly_pow(x + 1, 0, 1),
	write(' 3) '), deb_poly_pow(x + 1, 1, x + 1),
	write(' 4) '), deb_poly_pow(x - 1, 1, x - 1),
	write(' 5) '), deb_poly_pow(x - 1, 2, x^2 - 2*x + 1),
	write(' 6) '), deb_poly_pow(x + 1, 2, x^2 + 2*x + 1),
	write(' 7) '), deb_poly_pow(2*x + 1, 2, 4*x^2 + 4*x + 1),
	write(' 8) '), deb_poly_pow(2*x^2, 2, 4*x^4),
	write(' 9) '), deb_poly_pow(x^2, 2, x^4),
	write('10) '), deb_poly_pow(x + 1, 3, x^3 + 3*x^2 + 3*x + 1),
	write('11) '), deb_poly_pow(2*x^2 - 3*x^3 + 30, 1, 2*x^2 - 3*x^3 + 30),
	write('12) '), deb_poly_pow(2*x^2 - 3*x^3 + 30, 2, 9*x^6 - 12*x^5 + 4*x^4 - 180*x^3 + 120*x^2 + 900),
	write('13) '), deb_poly_pow(2*x^2 - 3*x^3 + 30, 3, -27*x^9 + 54*x^8 - 36*x^7 + 818*x^6 - 1080*x^5 + 360*x^4 - 8100*x^3 + 5400*x^2 + 27000),
	write('14) '), deb_poly_pow(2*x^2 - 3*x^3 + 30, 4, 81*x^12 - 216*x^11 + 216*x^10 - 3336*x^9 + 6496*x^8 - 4320*x^7 + 49560*x^6 - 64800*x^5 + 21600*x^4 - 324000*x^3 + 216000*x^2 + 810000),
	write('15) '), deb_poly_pow(2*x^2 - 3*x^3 + 30, 5, -243*x^15 + 810*x^14 - 1080*x^13 + 12870*x^12 - 32640*x^11 + 32432*x^10 - 257400*x^9 + 488400*x^8 - 324000*x^7 + 2502000*x^6 - 3240000*x^5 + 1080000*x^4 - 12150000*x^3 + 8100000*x^2 + 24300000),

	nl, write('- POLYNOMIAL EVALUATION -'), nl, nl,

	write(' 1) '), deb_poly_eval(x + x - 2, 2*x - 2),
	write(' 2) '), deb_poly_eval(x + x^2 - 2, x^2 + x - 2),
	write(' 3) '), deb_poly_eval(x*x^2 - 2, x^3 - 2),
	write(' 4) '), deb_poly_eval((x + 1)^2 - 1, x^2 + 2*x),
	write(' 4) '), deb_poly_eval(x*(x + 1)^2 - 1, x^3 + 2*x^2 + x - 1),

	true.