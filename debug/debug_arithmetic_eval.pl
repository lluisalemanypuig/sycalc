:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(common).

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

deb_arithm(I, E, RES):- arith_expr_eval(E, RES), !, output_correct(I).
deb_arithm(I, E, RES):-
	arith_expr_eval(E, R),
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
	deb_arithm('  4)', 2^2^2^2, 65536),
	deb_arithm('  5)', 2^(2^(2^(2))), 65536),
	deb_arithm('  6)', ((2^2)^2)^2, 256),
	% deb_arithm('  -)', 2^2^2^2^2, 65536), % the result is too large
	deb_arithm('  7)', (((2^2)^2)^2)^2, 65536),
	deb_arithm('  8)', 2^(1 + 1)^2, 16),
	deb_arithm('  9)', (2^2)^(2 + 1 - 1)^(2^2), 4294967296),
	deb_arithm(' 10)', (1/2)^2 + 3/4, 1),
	deb_arithm(' 11)', (1/2)^0, 1),
	deb_arithm(' 12)', (1/2)^0 + 3/4, 7/4),
	deb_arithm(' 13)', 1 - (1 + 1), -1),
	deb_arithm(' 14)', 1/2, 1/2),

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
