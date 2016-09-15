:-ensure_loaded(monomials).
:-ensure_loaded(lists).

% POLYNOMIALS
% A polynomial is a sum of monomials.
% These are monomials:
% 	x^3, -2*x, 6*x^2, x, -2
% These are not monomials:
% 	(x + 3)*(x - 2), x*x, x*y

polynomial_monomials(M, [R]):- monomial_red(M, R), !.
polynomial_monomials(A + B, S):- polynomial_monomials(A, L), polynomial_monomials(B, R), concat(L, R, S), !.
polynomial_monomials(A - B, S):- polynomial_monomials(A, L), polynomial_monomials(-B, R), concat(L, R, S), !.

polynomial_list([M], RM):- monomial_red(M, RM), !.
polynomial_list([M|L], M + S):- polynomial_list(L, S), !.

polynomial_eq(P1, P2):- polynomial_monomials(P1, M1), monomial_sort(M1, S1), polynomial_monomials(P2, M2), monomial_sort(M2, S1).

polynomial(P):- polynomial_monomials(P, _).

polynomial_degree(P, D):- polynomial_monomials(P, MS), map(monomial_degree, MS, DS), max(DS, D).
