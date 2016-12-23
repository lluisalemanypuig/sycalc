:-ensure_loaded(integer_algorithms).
:-ensure_loaded(monomials).
:-ensure_loaded(numbers).
:-ensure_loaded(lists).

% POLYNOMIALS
% A polynomial is a sum of monomials.
% These are monomials:
% 	x^3, -2*x, 6*x^2, x, -2
% These are not monomials:
% 	(x + 3)*(x - 2), x*x, x*y

polynomial_monomials(M, [R]):- red_monomial(M, R), !.
polynomial_monomials(A + B, S):- polynomial_monomials(A, L), polynomial_monomials(B, R), concat(L, R, S), !.
polynomial_monomials(A - B, S):- polynomial_monomials(A, L), polynomial_monomials(-B, R), concat(L, R, S), !.

first_monomial(A + B, A,     B):- monomial(A), monomial(B), !.
first_monomial(A - B, A,     N):- monomial(A), monomial(B), monomial_neg(B, N), !.
first_monomial(A + B, F, S + B):- first_monomial(A, F, S), !.
first_monomial(A - B, F, S - B):- first_monomial(A, F, S), !.
first_monomial(    F, F,     _).

last_monomial(A + B,     A, B):- monomial(B), !.
last_monomial(A - B,     A, N):- monomial(B), monomial_neg(B, N), !.

list_polynomial([M], M).
list_polynomial([M|L], S + M):- monomial_positive_coefficient(M), list_polynomial(L, S), !.
list_polynomial([M|L], S - N):- monomial_neg(M, N), list_polynomial(L, S), !.

polynomial_eq(P1, P2):- polynomial_monomials(P1, M1), monomial_sort(M1, S1), polynomial_monomials(P2, M2), monomial_sort(M2, S1).

polynomial(P):- polynomial_monomials(P, _).

polynomial_neg(P, N):- polynomial_monomials(P, L1), map(monomial_neg, L1, L2), polynomial_list(L2, N).

polynomial_degree(P, D):- polynomial_monomials(P, MS), map(monomial_degree, MS, DS), max(DS, D).

pretty_polynomial_roots([X], (x + XX)):- X < 0, rational_neg(X, XX).
pretty_polynomial_roots([X], (x - X)).
pretty_polynomial_roots([X|L], P*(x + XX)):- X < 0, rational_neg(X, XX), pretty_polynomial_roots(L, P), !.
pretty_polynomial_roots([X|L], P*(x - X)):- pretty_polynomial_roots(L, P), !.

ruffini([_], _, []):- !.
ruffini(CS, [D|_], [D|L]):-
	ladder_prod(D, 0, CS, RS, 0),
	write('RS='), write(RS), nl,
	last(RS, _, NC), divisors(NC, ND), ruffini(RS, ND, L), !.
ruffini(CS, [_|Ds], L):- ruffini(CS, Ds, L).

integer_roots_polynomial(P, R):-
	polynomial_monomials(P, M), monomial_inv_sort(M, MIS), last(MIS, _, L), monomial_coefficient(L, CF), divisors(CF, DVS),
	map(monomial_coefficient, MIS, MCFS),
	ruffini(MCFS, DVS, R).
