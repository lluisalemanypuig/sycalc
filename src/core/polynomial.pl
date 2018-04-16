:-ensure_loaded(list).
:-ensure_loaded(number).
:-ensure_loaded(monomial).

% POLYNOMIALS
% A polynomial is a sum of monomials.
% These are monomials:
%	x^3, -2*x, 6*x^2, x, -2, x*y
% These are not monomials:
%	(x + 3)*(x - 2),
% A polynomial may be expanded:
%   x^2 + 6*x + 6
% or contraced:
%   (x + 2)*(x + 3)
% Expanded polynomials may be represented as lists of monomials
%   x^2 - 3*x + 4 = [x^2, -3*x, 4]
% Some polynomials may be both contracted and expanded at the same time:
%   x + 3, -x^2, -9

% Returns the list of reduced monomials from a polynomial
list_from_polynomial(A + B, S):-
	list_from_polynomial(A, L), list_from_polynomial(B, R), list_concat(L, R, S), !.
list_from_polynomial(A - B, S):-
	list_from_polynomial(A, L), list_from_polynomial(-B, R), list_concat(L, R, S), !.
list_from_polynomial(M, [R]):- red_monomial(M, R), !.

% Pads a list of uni-variate* monomials DECREASINGLY sorted.
% If [m1, m2, ..., mN] is the list of monomials, this predicate will add 0s between
% those monomials with degrees di, dj such that |di - dj| > 1
% *: a polynomial is uni-variate if all monomials have the same variable (only one)
padded_unipoly_mons_decr([], []):- !.
padded_unipoly_mons_decr([M], [M]):- monomial_degrees(M, []), !.
padded_unipoly_mons_decr([M], R):-
	monomial_degrees(M, Ds), first(Ds, D, _),
	padded_list_end([M], D, 0, R), !.
padded_unipoly_mons_decr([M|Ms], R):-
	first(Ms, F, _), monomial_degrees(F, []), !,
	unimonomial_degree(M, D),
	K is D - 1,
	padded_list_end([M], K, 0, Q),
	list_concat(Q, Ms, R).
padded_unipoly_mons_decr([M|Ms], P):-
	unimonomial_degree(M, D1),
	first(Ms, F, _),
	unimonomial_degree(F, D2),
	K is D1 - D2 - 1,
	padded_list_end([M], K, 0, Q),    % K zeros between M and Ms
	padded_unipoly_mons_decr(Ms, R),
	list_concat(Q,R, P), !.

% A + B is an expanded polynomial.
% A is its first monomial
% B is the rest of the polynomial
polynomial_first_monomial(A + B, A,  B):- monomial(A), monomial(B), !.
polynomial_first_monomial(A - B, A, NB):- monomial(A), monomial(B), monomial_neg(B, NB), !.
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
polynomial_from_list_([], 0):- !.
polynomial_from_list_([M], M):- !.
polynomial_from_list_([M|L], S + M):-
	monomial_positive_coefficient(M), polynomial_from_list_(L, S), !.
polynomial_from_list_([M|L], S - N):-
	monomial_neg(M, N), polynomial_from_list_(L, S), !.
polynomial_from_list(M, P):-
	monomial_inv_sort(M, MS), polynomial_from_list_(MS, P).

% Compares two polynomials as list of monomials and fails if they are not equal
polynomial_list_eq(M1, M2):- monomial_sort(M1, S1), monomial_sort(M2, S1).

% Compares two expanded polynomials and fails if they are not equal
polynomial_eq(P1, P2):-
	list_from_polynomial(P1, M1), list_from_polynomial(P2, M2),
	polynomial_list_eq(M1, M2).

% Checks if P is an expanded polynomial
expanded_polynomial(P):- list_from_polynomial(P, _).

% P is an expanded polynomial. N = -P
polynomial_neg(P, N):-
	list_from_polynomial(P, L1), map(monomial_neg, L1, L2),
	polynomial_list(L2, N).

% P is an expanded polynomial. D is the maximum degree of its monomials
polynomial_degree(P, D):-
	list_from_polynomial(P, MS), map(monomial_degree, MS, DS), max(DS, D).

% Replace the polynomial's variable O with I
% The polynomial is passed as a list of monomials
polynomial_list_revar(O,I, P, Q):- map(monomial_revar(O,I), P, Q).

% recursive function called by pretty_polynomial_roots_
pretty_polynomial_roots_([[X,1]], (x + XX)):- X < 0, rational_neg(X, XX).
pretty_polynomial_roots_([[X,Po]], (x + XX)^Po):- X < 0, rational_neg(X, XX).
pretty_polynomial_roots_([[X,1]], (x - X)):- !.
pretty_polynomial_roots_([[X,Po]], (x - X)^Po):- !.
pretty_polynomial_roots_([[X,1]|L], P*(x + XX)):-
	X < 0, rational_neg(X, XX), pretty_polynomial_roots_(L, P), !.
pretty_polynomial_roots_([[X,Po]|L], P*((x + XX)^Po)):-
	X < 0, rational_neg(X, XX), pretty_polynomial_roots_(L, P), !.
pretty_polynomial_roots_([[X,1]|L], P*(x - X)):-
	pretty_polynomial_roots_(L, P), !.
pretty_polynomial_roots_([[X,Po]|L], P*((x - X)^Po)):-
	pretty_polynomial_roots_(L, P), !.

% Given a list of rationals R, builds a univariate contracted polynomial
% P on variable 'x' with these numbers as its roots.
pretty_polynomial_roots(R, P):- how_many(R, C), pretty_polynomial_roots_(C, P).

% ruffini(C, D, R)
% Given the list of integers C (coefficients of the monomials sorted
% DECREASINGLY), the list of divisors D of the last coefficient (that is
% a free term) applies Ruffini's method for polynomial factorization and
% obtains the list of roots R.
%
% The list of integers (coefficients of the monomials sorted DECREASINGLY)
% must be from a polynomial with ONLY integer roots.
ruffini([_], _, []):- !.
ruffini(CS, [D|_], [D|L]):-
	ladder_prod(D, 0, CS, RS, 0), last(RS, _, NC), divisors(NC, ND),
	ruffini(RS, ND, L), !.
ruffini(CS, [_|Ds], L):- ruffini(CS, Ds, L), !.

% Finds all the integer roots of a univariate polynomial P.
% This predicate is simply a wrapper that calls 'ruffini/3' predicate.
% This polynomial should have one free term (a constant multiplied by x^0)
% and the first monomial be multiplied by 1 or -1.
integer_roots_polynomial(P, R):-
	% extract the padded monomial list of P
	list_from_polynomial(P, M), monomial_sort(M, SM), padded_unipoly_mons_decr(SM, PAD_SM),

	% obtain all the divisors of the free term
	last(PAD_SM, _, L), monomial_coefficient(L, CF), divisors(CF, DVS),

	% apply ruffini's algorithm
	map(monomial_coefficient, PAD_SM, MCFS), ruffini(MCFS, DVS, R).

