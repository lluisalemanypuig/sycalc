:-ensure_loaded(lists).

% POLYNOMIALS
% A polynomial is a reduced sum of reduced monomials.
% These are monomials:
% 	x^3, -2*x, 6*x^2, x, -2
% These are not monomials:
% 	(x + 3)*(x - 2), x*x, x*y

polynomial_monomials(M, [R]):- monomial_red(M, R), !.
polynomial_monomials(A + B, S):- polynomial_monomials(A, L), polynomial_monomials(B, R), concat(L, R, S), !.
polynomial_monomials(A - B, S):- polynomial_monomials(A, L), polynomial_monomials(-B, R), concat(L, R, S), !.

polynomial_list([M], RM):- monomial_red(M, RM), !.
polynomial_list([M|L], M + S):- polynomial_list(L, S), !.

polynomial_eq(P1, P2):- polynomial_monomials(P1, M1), polynomial_monomials(P2, M1).

polynomial(P):- polynomial_monomials(P, _).

polynomial_degree(P, D):- polynomial_monomials(P, MS), map(monomial_degree, MS, DS), max(DS, D).

degree_comp(M1, M2):- monomial_degree(M1, D1), monomial_degree(M2, D2), D1 < D2.
sort_mon_degree(L, R):- isort_by(degree_comp, L, R).

list_monomials_reduced([], []).
list_monomials_reduced([M], [RM]):- monomial_red(M, RM), !.
list_monomials_reduced([M1,M2], [M1,M2]):- mon_sum(M1, M2, S), S == M1 + M2, !.
list_monomials_reduced([M1,M2], [S]):- mon_sum(M1, M2, S), !.
list_monomials_reduced([M1,M2|L], [M1|R]):- mon_sum(M1, M2, R), R == M1 + M2, list_monomials_reduced([M2|L], R), !.
list_monomials_reduced([M1,M2|L], Q):- mon_sum(M1, M2, S), list_monomials_reduced([S|L], R), list_monomials_reduced(R, Q), !.
list_monomials_reduced(X, X).

polynomial_reduced(P, PR):-
	polynomial_monomials(P, M), sort_mon_degree(M, R),
	list_monomials_reduced(R, LR), polynomial_list(LR, PR).

%%%%%%%%%%%%
% ----------
% DEBUG

red_poly(P, TAB, RES):- write(P), write(': '), polynomial_reduced(P, R), write(R), write(TAB), write(' | correct? '), polynomial_eq(R, RES), write('Yes'), !, nl.
red_poly(_,   _,   _):- write('No'), nl, false.

debug_polynomials:-
	nl, write('-- POLYNOMIALS DEBUG --'), nl,
	nl, write('- POLYNOMIAL REDUCTION -'), nl, nl,

	write(' 1) '), red_poly(0*x^0,     '             ', 0),
	write(' 2) '), red_poly(0 + x + 0, '             ', x),
	write(' 3) '), red_poly(0 + x^2 + x^(1 + 1), '', 2*x^2),
	write(' 4) '), red_poly(x^2 - 2*x^(3 - 1), ' ', -x^2),
	write(' 5) '), red_poly((1/2)*x - (1/2)*x, '       ', 0),
	write(' 6) '), red_poly((1/2)*x - (3/2)*x , '      ', -x),
	write(' 7) '), red_poly((1/2)*x + (3/2)*x , '     ', 2*x),
	write(' 8) '), red_poly(0 + (1/2)*x + (3/2)*x , '   ', 2*x),
	write(' 9) '), red_poly(0 + (1/2)*x - 0 + (3/2)*x , ' ', 2*x),
	write(' 10) '), red_poly(x^2 + (1/2)*x - 0 + (3/2)*x , ' ', 2*x + x^2),
	write(' 11) '), red_poly(x - x^2 + x^3 - x^4 + x^5, '   ', x - x^2 + x^3 - x^4 + x^5),
	write(' 12) '), red_poly((1/3)*x^4 + (1/2)*x^3 + (1/6)*x^2 - (1/6)*x^3 + (1/4)*x^2 - (1/12)*x - (1/12)*x^2 + (1/12)*x - x^3, '     ', (1/3)*x^2 - (2/3)*x^3 + (1/3)*x^4),

	true.