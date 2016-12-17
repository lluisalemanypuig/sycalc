:-ensure_loaded(list).
:-ensure_loaded(number).
:-ensure_loaded(monomial).

% POLYNOMIALS
% A polynomial is a sum of monomials.
% These are monomials:
% 	x^3, -2*x, 6*x^2, x, -2
% These are not monomials:
% 	(x + 3)*(x - 2), x*x, x*y
% A polynomial may be expanded:
%   x^2 + 6*x + 6
% or contraced:
%   (x + 2)*(x + 3)
% Expanded polynomials may be represented as lists of monomials
%   x^2 - 3*x + 4 = [x^2, -3*x, 4]
% Some polynomials may be both contracted and expanded at the same time:
%   x + 3, -x^2, -9

% Returns the polynomials' monomials as a list of reduced monomials
polynomial_monomials(A + B, S):- polynomial_monomials(A, L), polynomial_monomials(B, R), concat(L, R, S), !.
polynomial_monomials(A - B, S):- polynomial_monomials(A, L), polynomial_monomials(-B, R), concat(L, R, S), !.
polynomial_monomials(M, [R]):- red_monomial(M, R), !.

% Pads a list of monomials DECREASINGLY sorted.
% If [m1, m2, ..., mN] is the list of monomials, this predicate will add 0s between
% those monomials with degrees di, dj such that |di - dj| > 1
padded_poly_mons_incr([], []).
padded_poly_mons_incr([M], R):- monomial_degree(M, D), D > 0, padded_list([M], D, 0, R), !.
padded_poly_mons_incr([M], [M]):- !.
padded_poly_mons_incr([M|MS], [M|P]):- first(MS, F, _), monomial_degree(M, D1), monomial_degree(F, D2), 1 is D1 - D2, padded_poly_mons_incr(MS, P), !.
padded_poly_mons_incr([M|MS], P):-
	first(MS, F, _), monomial_degree(M, D1), monomial_degree(F, D2), K is D1 - D2 - 1,
	padded_list([M], K, 0, R), padded_poly_mons_incr(MS, Q), concat(R, Q, P), !.

% A + B is an expanded polynomial.
% A is its first monomial
% B is the rest of the polynomial
polynomial_first_monomial(A + B, A, B):- monomial(A), monomial(B), !.
polynomial_first_monomial(A - B, A, N):- monomial(A), monomial(B), monomial_neg(B, N), !.
polynomial_first_monomial(A + B, F, S + B):- polynomial_first_monomial(A, F, S), !.
polynomial_first_monomial(A - B, F, S - B):- polynomial_first_monomial(A, F, S), !.
polynomial_first_monomial(F, F, _).

% A + B is an expanded polynomial.
% A is the rest of the polynomial
% B is its last monomial
polynomial_last_monomial(A + B, A, B):- monomial(B), !.
polynomial_last_monomial(A - B, A, N):- monomial(B), monomial_neg(B, N), !.

% Builds an expanded polynomial from a list of reduced monomials.
% The last monomial will be the first in the polynomial
list_polynomial([M], M):- !.
list_polynomial([M|L], S + M):- monomial_positive_coefficient(M), list_polynomial(L, S), !.
list_polynomial([M|L], S - N):- monomial_neg(M, N), list_polynomial(L, S), !.

% Compares two expanded polynomials and fails if they are not equal
polynomial_eq(P1, P2):- polynomial_monomials(P1, M1), monomial_sort(M1, S1), polynomial_monomials(P2, M2), monomial_sort(M2, S1).

% Checks if P is an expanded polynomial
polynomial(P):- polynomial_monomials(P, _).

% P is an expanded polynomial. N = -P
polynomial_neg(P, N):- polynomial_monomials(P, L1), map(monomial_neg, L1, L2), polynomial_list(L2, N).

% P is an expanded polynomial. D is the maximum degree of its monomials
polynomial_degree(P, D):- polynomial_monomials(P, MS), map(monomial_degree, MS, DS), max(DS, D).

pretty_polynomial_roots_([[X,1]], (x + XX)):- X < 0, rational_neg(X, XX).
pretty_polynomial_roots_([[X,Po]], (x + XX)^Po):- X < 0, rational_neg(X, XX).
pretty_polynomial_roots_([[X,1]], (x - X)):- !.
pretty_polynomial_roots_([[X,Po]], (x - X)^Po):- !.
pretty_polynomial_roots_([[X,1]|L], P*(x + XX)):- X < 0, rational_neg(X, XX), pretty_polynomial_roots_(L, P), !.
pretty_polynomial_roots_([[X,Po]|L], P*((x + XX)^Po)):- X < 0, rational_neg(X, XX), pretty_polynomial_roots_(L, P), !.
pretty_polynomial_roots_([[X,1]|L], P*(x - X)):- pretty_polynomial_roots_(L, P), !.
pretty_polynomial_roots_([[X,Po]|L], P*((x - X)^Po)):- pretty_polynomial_roots_(L, P), !.

% pretty_polynomial_roots(L, P):
% Given a list of rationals L, buils a contracted polynomial P with these numbers as its roots.
pretty_polynomial_roots(R, P):- how_many(R, C), pretty_polynomial_roots_(C, P).

% ruffini(C, D, R)
% Given the list of integers C (coefficients of the monomials sorten DECREASINGLY),
% the list of divisors D of the last coefficient (that is a free term)
% applies Ruffini's method for polynomial factorization and obtains the list of roots R.
%
% The list of integers (coefficients of the monomials sorten DECREASINGLY) must be from
% a polynomial with ONLY integer roots.
ruffini([_], _, []):- !.
ruffini(CS, [D|_], [D|L]):- ladder_prod(D, 0, CS, RS, 0), last(RS, _, NC), divisors(NC, ND), ruffini(RS, ND, L), !.
ruffini(CS, [_|Ds], L):- ruffini(CS, Ds, L), !.

% Finds all the integer roots of the polynomial P.
% This predicate is simply a wrapper that calls 'ruffini/3' predicate.
% This polynomial should have one free term (a constant multiplied by x^0)
% and the first monomial be multiplied by 1 or -1.
integer_roots_polynomial(P, R):-
	polynomial_monomials(P, M), monomial_inv_sort(M, MIS), padded_poly_mons_incr(MIS, PAD_MIS),	% extract the padded monomial list of P
	last(PAD_MIS, _, L), monomial_coefficient(L, CF), divisors(CF, DVS), 						% obtain all the divisors of the free term
	map(monomial_coefficient, PAD_MIS, MCFS), ruffini(MCFS, DVS, R).
