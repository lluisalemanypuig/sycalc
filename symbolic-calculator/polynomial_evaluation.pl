:-ensure_loaded(polynomials).
:-ensure_loaded(lists).

% takes a polynomial as a list of monomials and reduces it
list_monomials_sum([], []).
list_monomials_sum([M], [RM]):- monomial_red(M, RM), !.
list_monomials_sum([M1,M2], [S]):- mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)), !.
list_monomials_sum([M1,M2], [M1,M2]):- !.
list_monomials_sum([M1,M2|L], R):- mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)), list_monomials_sum([S|L], R), !.
list_monomials_sum([M1,M2|L], [M1|R]):- list_monomials_sum([M2|L], R), !.
list_monomials_sum(X, X).

% takes a polynomial as a list and returns it as a reduced list of monomials
list_polynomial_sum_list(L, LR):- monomial_sort(L, R), list_monomials_sum(R, LR).

% takes a polynomial and reduces it
polynomial_sum(P, PR):-
	polynomial_monomials(P, M), list_polynomial_sum_list(M, LR),
	polynomial_list(LR, PR).

% takes two polynomials each of them as a list, multiplies them and returns it as
% a reduced list of monomials
list_polynomial_prod_list(L1, L2, L):-
	cartesian_product(L1, L2, CP), map(mon_prod, CP, MON_PROD),
	list_polynomial_sum_list(MON_PROD, L).

% takes two polynomials and multiplies them
polynomial_prod(P1, P2, P):-
	polynomial_monomials(P1, L1), polynomial_monomials(P2, L2),
	list_polynomial_prod_list(L1, L2, MON_PROD), polynomial_list(MON_PROD, P).

% takes a polynomial as a list, an integer number and performs the power P^N
list_polynomial_power_list(_, 0, [1]):- !.
list_polynomial_power_list(L, 1, L):- !.
list_polynomial_power_list(LP, N, LN):-
	natural(N), N1 is N - 1, list_polynomial_power_list(LP, N1, L),
	list_polynomial_prod_list(LP, L, LN).

% takes a polynomial, an integer number and performs the power P^N
polynomial_power(_, 0, 1):- !.
polynomial_power(P, 1, P):- !.
polynomial_power(P, N, PN):-
	polynomial_monomials(P, M), list_polynomial_power_list(M, N, L),
	polynomial_list(L, PN).

% POLYNOMIAL EXPRESSIONS' EVALUATION

polynomial_evaluation_list(Q1 + Q2, R):-
	polynomial_evaluation_list(Q1, L1), polynomial_evaluation_list(Q2, L2),
	concat(L1, L2, L), list_polynomial_sum_list(L, R), !.
polynomial_evaluation_list(Q1 * Q2, R):-
	polynomial_evaluation_list(Q1, L1), polynomial_evaluation_list(Q2, L2),
	cartesian_product(L1, L2, L), map(mon_prod, L, PROD),
	list_polynomial_sum_list(PROD, R), !.
polynomial_evaluation_list(Q1 ^ N, R):-
	polynomial_evaluation_list(Q1, L1), list_polynomial_power_list(L1, N, R), !.
polynomial_evaluation_list(P, [R]):- monomial_red(P, R), !.

polynomial_evaluation(P, R):- polynomial_evaluation_list(P, L), polynomial_list(L, R).

