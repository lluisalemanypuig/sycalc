:-ensure_loaded("../src/core").
:-ensure_loaded("../src/algorithms").
:-ensure_loaded(common).

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