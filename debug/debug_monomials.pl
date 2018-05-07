:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(common).

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
	monomial_value_evaluation(VAL, V, M, R), RES == R, !, output_correct(I).
deb_mon_eval(I, M, V, VAL, RES):-
	monomial_value_evaluation(VAL, V, M, R),
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