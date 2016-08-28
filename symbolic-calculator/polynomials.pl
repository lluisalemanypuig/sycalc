:-include(lists).

% POLYNOMIALS
% A polynomial is a sum of monomials.
% These are monomials:
% 	x^3, -2*x, 6*x^2, x, -2
% These are not monomials:
% 	(x + 3)*(x - 2)

polynomial_monomials(M, [R]):- monomial_reduced(M, R), !.
polynomial_monomials(A + B, S):- polynomial_monomials(A, L), polynomial_monomials(B, R), concat(L, R, S), !.
polynomial_monomials(A - B, S):- polynomial_monomials(A, L), polynomial_monomials(-B, R), concat(L, R, S), !.
polynomial_monomials(E, _):-
	write('Error (polynomial_monomials): error when parsing monomials. Monomial received: '),
	write(E), nl, false.

polynomial_list([M], RM):- monomial_reduced(M, RM), !.
polynomial_list([M|L], M + S):- polynomial_list(L, S), !.

polynomial(P):- polynomial_monomials(P, _).

polynomial_degree(P, D):- polynomial_monomials(P, MS), map(monomial_degree, MS, DS), max(DS, D).

degree_comp(M1, M2):- monomial_degree(M1, D1), monomial_degree(M2, D2), D1 < D2.
sort_mon_degree(L, R):- isort_by(degree_comp, L, R).

list_monomials_reduced([M], [RM]):- monomial_reduced(M, RM), !.
list_monomials_reduced([M1,M2|L], Q):-
	monomial_degree(M1, D1), monomial_degree(M2, D2),
	D1 == D2,
	monomial_sum(M1, M2, S),
	list_monomials_reduced([S|L], R), !,
	list_monomials_reduced(R, Q), !.

list_monomials_reduced([M1,M2|L], [M1|R]):-
	list_monomials_reduced([M2|L], R), !.

polynomial_reduced(P, PR):-
	polynomial_monomials(P, M), sort_mon_degree(M, R),
	list_monomials_reduced(R, LR), polynomial_list(LR, PR).
