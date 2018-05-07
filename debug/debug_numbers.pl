:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(common).

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