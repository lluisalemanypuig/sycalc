/*********************************************************************
 * Sycalc
 * Copyright (C) 2018  Lluís Alemany Puig
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Contact: Lluís Alemany Puig (lluis.alemany.puig@gmail.com)
 * 
 ********************************************************************/

:-ensure_loaded(list).
:-ensure_loaded(number).
:-ensure_loaded(monomial).

/***
	@descr This file contains the definition of multivariate polynomials
	and provides predicates for arithmetic operations and comparison.
	
	
	A multivariate polynomial is a summation of multivariate monomials.
	If all monomials are univariate then the polynomial is univariate.
	
	
	Examples of polynomials are:
	\bverbatim
		x + 3, -x^2 - 9
	\everbatim
	
	Polynomials can only be operated with other polynomials when they are
	represented as lists of reduced monomials. This list can be obtained
	using predicate ?list_from_polynomial/2. However, some predicates
	need the polynomial to be in expanded form. An expanded polynomial
	is given as a sum of monomials as in the examples given above.
	
	
	Henceforth, a univariate polynomial will be referred to as 'unipoly'.
*/

/**
	@form list_from_polynomial(Poly, MonList)
	@descr Obtains the list of monomials @MonList in polynomial @Poly.
*/
list_from_polynomial(A + B, S):-
	list_from_polynomial(A, L), list_from_polynomial(B, R), list_concat(L, R, S), !.
list_from_polynomial(A - B, S):-
	list_from_polynomial(A, L), list_from_polynomial(-B, R), list_concat(L, R, S), !.
list_from_polynomial(M, [R]):- red_monomial(M, R), !.

/**
	@form polynomial_first_monomial(Poly, FirstMon, RestMon)
	@descr @FirstMon is the first monomial of polynomial @Poly. @RestMon
	are the oher monomials of @Poly.
	@constrs
		@param Poly Is an expanded polynomial of the form
		\blist
		\item A + B
		\item A - B
		\item C
		++>
		where B is a monomial, and A is either a monomial or a polynomial.
		@Poly is of the form of C when it is a single monomial.
*/
polynomial_first_monomial(A + B, A,  B):- monomial(A), monomial(B), !.
polynomial_first_monomial(A - B, A, NB):- monomial(A), monomial(B), monomial_neg(B, NB), !.
polynomial_first_monomial(A + B, F, S + B):- polynomial_first_monomial(A, F, S), !.
polynomial_first_monomial(A - B, F, S - B):- polynomial_first_monomial(A, F, S), !.
polynomial_first_monomial(F, F, _).

/**
	@form polynomial_last_monomial(Poly, RestMon, LastMon)
	@descr @LastMon is the last monomial of @Poly. @RestMon are the other
	monomials.
	@constrs
		@param Poly Is an expanded polynomial of the form
		\blist
		\item A + B
		\item A - B
		\item C
		++>
		where B is a monomial, and A is either a monomial or a polynomial.
		@Poly is of the form of C when it is a single monomial.
*/
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
/**
	@form polynomial_from_list(MonList, Poly)
	@descr @Poly is an expanded polynomial containing the monomials in
	@MonList (basically, their summation).
*/
polynomial_from_list(M, P):-
	monomial_inv_sort(M, MS), polynomial_from_list_(MS, P).

/**
	@form polynomial_list_eq(MonList1, MonList2)
	@descr This predicate fails if @MonList1 and @MonList2 are not equal.
*/
polynomial_list_eq(M1, M2):- monomial_sort(M1, S1), monomial_sort(M2, S1).

/**
	@form polynomial_eq(Poly1, Poly2)
	@descr This predicate fails if @Poly1 and @Poly2 are not equal.
*/
polynomial_eq(P1, P2):-
	list_from_polynomial(P1, M1), list_from_polynomial(P2, M2),
	polynomial_list_eq(M1, M2).

/**
	@form expanded_polynomial(Poly)
	@descr This predicate fails if @Poly is not an expanded polynomial.
*/
expanded_polynomial(P):- list_from_polynomial(P, _).

/**
	@form polynomial_neg(Poly, NegPoly)
	@descr @NegPoly equals -@Poly.
*/
polynomial_neg(P, N):-
	list_from_polynomial(P, L1), map(monomial_neg, L1, L2),
	polynomial_from_list(L2, N).

/**
	@form polynomial_list_revar(Var, With, Poly, Result)
	@descr @Result is the polynomial resulting from the replacement of
	@Poly's variable @Var with variable @With.
*/
polynomial_list_revar(O,I, P, Q):- map(monomial_revar(O,I), P, Q).

/*! The following predicates are restricted to unipolynomials only. */

/**
	@form padded_unipoly_mons_decr(MonList, PaddedList)
	@descr Let [m1, m2, ..., mN] be the list of monomials @MonList.
	@PaddedList is the list that results from inserting zeroes
	between those monomials in @MonList with degrees di, dj such that
	|di - dj| > 1.
	@constrs
		@param MonList A list of unimonomials decreasingly sorted.
*/
padded_unipoly_mons_decr([], []):- !.
padded_unipoly_mons_decr([M], [M]):- monomial_degrees(M, []), !.
padded_unipoly_mons_decr([M], R):-
	unimonomial_degree(M, D),
	pad_end(D, [M], 0, R), !.
padded_unipoly_mons_decr([M|Ms], R):-
	first(Ms, F, _), monomial_degrees(F, []), !,
	unimonomial_degree(M, D),
	K is D - 1,
	pad_end(K, [M], 0, Q),
	list_concat(Q, Ms, R).
padded_unipoly_mons_decr([M|Ms], P):-
	unimonomial_degree(M, D1),
	first(Ms, F, _),
	unimonomial_degree(F, D2),
	K is D1 - D2 - 1,
	pad_end(K, [M], 0, Q),    % K zeros between M and Ms
	padded_unipoly_mons_decr(Ms, R),
	list_concat(Q,R, P), !.

/**
	@form unipolynomial_degree(UniPoly, Degree)
	@descr @Degree is the degree of the univariate polynomial @UniPoly.
*/
unipolynomial_degree(P, D):-
	list_from_polynomial(P, MS), map(unimonomial_degree, MS, DS), max(DS, D).

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

/**
	@form pretty_polynomial_roots(Roots, Poly)
	@descr @Poly is a contracted polynomial with roots in @Roots.
	
	For example, when given the values @Roots = [1,2,3], @Poly is the
	polynomial
	\bverbatim
		(x-3)* (x-2)* (x-1)
	\everbatim
*/
pretty_polynomial_roots(R, P):- how_many(R, C), pretty_polynomial_roots_(C, P).

/**
	@form ruffini(Coefs, Divs, Roots)
	@descr @Roots is the list of roots of the polynomial with coefficients
	@Coefs, where:
	\blist
	\item @Coefs is a list of integer values representing the coefficients
	of the monomials of a unipolynomial.
	\item @Divs is a list of divisors of the last value in @Coefs
	\elist
	@constrs
		@param Coefs The list of coefficients is sorted decreasingly.
		@param Roots Will contian only the integer roots of the polynomial.
*/
ruffini([_], _, []):- !.
ruffini(CS, [D|_], [D|L]):-
	ladder_prod(D, 0, CS, RS, 0), last(RS, _, NC), divisors(NC, ND),
	ruffini(RS, ND, L), !.
ruffini(CS, [_|Ds], L):- ruffini(CS, Ds, L), !.

/**
	@form integer_roots_unipolynomial(Poly, Roots)
	@descr @Roots is a list of integer values for which @Poly is evaluated
	to zero, that is, the integer roots of @Poly. Uses predicate ?ruffini/3.
*/
integer_roots_unipolynomial(P, R):-
	% extract the padded monomial list of P
	list_from_polynomial(P, M), monomial_sort(M, SM), padded_unipoly_mons_decr(SM, PAD_SM),

	% obtain all the divisors of the free term
	last(PAD_SM, _, L), monomial_coefficient(L, CF), divisors(CF, DVS),

	% apply ruffini's algorithm
	map(monomial_coefficient, PAD_SM, MCFS), ruffini(MCFS, DVS, R).

