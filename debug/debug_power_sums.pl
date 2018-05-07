:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(common).

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