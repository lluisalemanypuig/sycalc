:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(common).

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
