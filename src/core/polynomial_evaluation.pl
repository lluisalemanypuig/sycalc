/*********************************************************************
 * Sycalc
 * Copyright (C) 2018,2019,2020  Lluís Alemany Puig
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

:-ensure_loaded(polynomial).
:-ensure_loaded(list).

/***
	@descr This file contains predicates that define simple operations
	with reduced polynomials.
	
	
	In this file is used for the first time the concept of reduced
	polynomial. A reduced polynomial is a polynomial for which the sum
	of any pair of its monomials does not result into a single monomial.
	These are reduced polynomials:
	\bverbatim
	3*x*y + 3*x^2
	4*x + 5*z^3 - 4
	\everbatim
	These are not reduced polynomials
	\bverbatim
	3*x + 3*y - x
	3*x^2 - 3*x*z^3 + y + x*z^3
	\everbatim
	
	Also, we defined the concepts of contracted and expanded polynomials.
	An expanded polynomial is of the form of those in the examples above.
	An expanded polynomial is a sum of monomials.
	Informally, an expanded polynomial cannot have parenthesised expressions
	like:
	\bverbatim
	3*(x + 3) - 4*x + 5*(x - 3)*(y + 2)
	\everbatim
	
	A contracted polynomial is a not fully expanded polynomial. Therefore
	they may contain parenthesised expressions.
*/

/**
	@form red_sorted_list_monomials(List, RedList)
	@descr @RedList is the reduction of @List by the arithmetic sum
	of the monomials in @List.
	
	These are not reduced lists of polynomials:
	\bverbatim
		[x^2,-2*x^2, x, y]
		[x^3, 0, 1]
	\everbatim
	These are reduced lists of polynomials:
	\bverbatim
		[x^3, 1]
		[x^3, 4*x^2, 1]
	\everbatim
	@constrs
		@param List Must be sorted
*/
red_sorted_list_monomials([], []):- !.
red_sorted_list_monomials([0], []):- !.
red_sorted_list_monomials([M], [RM]):- red_monomial(M, RM), !.
red_sorted_list_monomials([M1,M2], []):- mon_sum(M1, M2, R), R = 0, !.
red_sorted_list_monomials([M1,M2], [S]):- mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)), !.
red_sorted_list_monomials([M1,M2], [M1,M2]):- !.
red_sorted_list_monomials([M1,M2|L], R):-
	mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)),
	red_sorted_list_monomials([S|L], R), !.
red_sorted_list_monomials([M1,M2|L], [M1|R]):- red_sorted_list_monomials([M2|L], R), !.
red_sorted_list_monomials(X, X).

/**
	@form red_sorted_list_monomials(List, RedList)
	@descr Similar to ?red_sorted_list_monomials/2 but @List is not
	necessarily sorted.
*/
red_list_monomials(L, LR):-
	monomial_sort(L, R), red_sorted_list_monomials(R, LR).

%---------------
%%	SUM

/*! Addition of two polynomials */

/**
	@form polynomial_reduction(Poly, RedPoly)
	@descr @RedPoly is the reduction of polynomial @Poly.
	@constrs
		@param Poly An expanded multivariate polynomial.
*/
polynomial_reduction(P, R):-
	list_from_polynomial(P, M),
	red_list_monomials(M, L),
	polynomial_from_list(L, R).

% Takes two uni-variate polynomials each of them as reduced and
% decreasingly sorted lists of monomials of unique degree and adds the
% second from to first and returns it as a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
unipoly_from_list_sum_sorted_list__(P1, D1, P2, D2, R):- D1 < D2, !,
	NZEROES is D2 - D1,
	pad_begin(NZEROES, P1, 0, PP1),
	zip_with(mon_sum, PP1, P2, R).
unipoly_from_list_sum_sorted_list__(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2,
	pad_begin(NZEROES, P2, 0, PP2),
	zip_with(mon_sum, P1, PP2, R).

unipoly_from_list_sum_sorted_list_(PP1, PP2, R):-
	first(PP1, M1, _),
	first(PP2, M2, _),
	unimonomial_degree(M1, D1),
	unimonomial_degree(M2, D2),
	unipoly_from_list_sum_sorted_list__(PP1, D1, PP2, D2, R).

/**
	@form unipoly_from_list_sum_sorted_list(Poly1, Poly2, RedPoly)
	@descr @RedPoly is the addition of polynomials @Poly1 and @Poly2.
	@RedPoly is a reduced list of monomials.
	@constrs Both @Poly1 and @Poly2 are given as sorted reduced lists
	of monomials, and are univariate on the same variable.
*/
unipoly_from_list_sum_sorted_list(P, [], P):- !.
unipoly_from_list_sum_sorted_list([], P, P):- !.
unipoly_from_list_sum_sorted_list(P1, P2, R):-
	padded_unipoly_mons_decr(P1, PP1),
	padded_unipoly_mons_decr(P2, PP2),
	unipoly_from_list_sum_sorted_list_(PP1, PP2, LR),
	red_sorted_list_monomials(LR, R), !.

/**
	@form polynomial_from_list_sum_list(Poly1, Poly2, RedPoly)
	@descr @RedPoly is the addition of polynomials @Poly1 and @Poly2.
	@RedPoly is a reduced list of monomials.
	
	Similar to ?unipoly_from_list_sum_sorted_list/3 but the lists are
	not necessarily sorted and the polynomials may be multivariate.
	@constrs Both @Poly1 and @Poly2 are given as lists of monomials.
*/
polynomial_from_list_sum_list(P1, P2, R):-
	list_concat(P1, P2, P),
	red_list_monomials(P, R).

%---------------
%%	SUB

/*! Substraction of two polynomials */

% Takes two uni-variate polynomials each of them as reduced and
% DECREASINGLY sorted lists of monomials of unique degree and substracts
% the second from the first and returns it as a reduced list of
% monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
unipoly_from_list_sub_sorted_list(P1, D1, P2, D2, R):- D1 < D2,
	NZEROES is D2 - D1,
	pad_begin(NZEROES, P1, 0, PP1),
	zip_with(mon_sub, PP1, P2, R), !.
unipoly_from_list_sub_sorted_list(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2,
	pad_begin(NZEROES, P2, 0, PP2),
	zip_with(mon_sub, P1, PP2, R).

/**
	@form unipoly_from_list_sub_sorted_list(Poly1, Poly2, RedPoly)
	@descr @RedPoly is the substrction of @Poly2 from @Poly1.
	@RedPoly is a reduced list of monomials.
	@constrs Both @Poly1 and @Poly2 are given as sorted reduced lists
	of monomials, and are univariate on the same variable.
*/
unipoly_from_list_sub_sorted_list(P1, [], P1):- !.
unipoly_from_list_sub_sorted_list([], P2, R):- map(monomial_neg, P2, R), !.
unipoly_from_list_sub_sorted_list(P1, P2, R):-
	padded_unipoly_mons_decr(P1, PP1),
	padded_unipoly_mons_decr(P2, PP2),
	first(PP1, FMON1, _), first(PP2, FMON2, _),
	unimonomial_degree(FMON1, D1),
	unimonomial_degree(FMON2, D2),
	unipoly_from_list_sub_sorted_list(PP1, D1, PP2, D2, LR),
	red_sorted_list_monomials(LR, R), !.

/**
	@form polynomial_from_list_sub_list(Poly1, Poly2, RedPoly)
	@descr @RedPoly is the substraction of polynomials @Poly2 from @Poly1.
	@RedPoly is a reduced list of monomials.
	
	Similar to ?unipoly_from_list_sub_sorted_list/3 but the lists are
	not necessarily sorted and the polynomials may be multivariate.
	@constrs Both @Poly1 and @Poly2 are given as lists of monomials.
*/
polynomial_from_list_sub_list(P1, P2, R):-
	map(monomial_neg, P2, NP2),
	polynomial_from_list_sum_list(P1, NP2, R).

%---------------
%%	PROD

/*! Product of polynomials */

% Takes two uni-variate polynomials each of them as reduced and
% DECREASINGLY sorted lists of monomials of unique degree and multiplies
% them. The result is a list of reduced monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
unipoly_from_list_prod_sorted_list_([], _, [[]]):- !.
unipoly_from_list_prod_sorted_list_(_, [], [[]]):- !.
unipoly_from_list_prod_sorted_list_([M], L2, [P]):-
	map(mon_prod(M), L2, P), !.
unipoly_from_list_prod_sorted_list_([M|L], L2, [P|Q]):-
	map(mon_prod(M), L2, P),
	unipoly_from_list_prod_sorted_list_(L, L2, Q), !.

/**
	@form unipoly_from_list_prod_sorted_list(Poly1, Poly2, RedPoly)
	@descr @RedPoly is the product of @Poly1 and @Poly2.
	@RedPoly is a reduced list of monomials.
	@constrs Both @Poly1 and @Poly2 are given as sorted reduced lists
	of monomials, and are univariate on the same variable.
*/
unipoly_from_list_prod_sorted_list([], [], []):- !.
unipoly_from_list_prod_sorted_list([],  _, []):- !.
unipoly_from_list_prod_sorted_list( _, [], []):- !.
unipoly_from_list_prod_sorted_list(L1, L2, L):-
	unipoly_from_list_prod_sorted_list_(L1, L2, ML),
	foldl(unipoly_from_list_sum_sorted_list, [], ML, LR),
	red_sorted_list_monomials(LR, L), !.

/**
	@form polynomial_from_list_prod_list(Poly1, Poly2, RedPoly)
	@descr @RedPoly is the product of polynomials @Poly2 from @Poly1.
	@RedPoly is a reduced list of monomials.
	
	Similar to ?unipoly_from_list_prod_sorted_list/3 but the lists are
	not necessarily sorted and the polynomials may be multivariate.
	@constrs Both @Poly1 and @Poly2 are given as lists of monomials.
*/
polynomial_from_list_prod_list(L1, L2, L):-
	cartesian_product_by(mon_prod, L1, L2, MON_PROD),
	red_list_monomials(MON_PROD, L).

/**
	@form polynomial_prod(Poly1, Poly2, RedPoly)
	@descr @RedPoly is the product of polynomials @Poly2 from @Poly1.

	An example of usage is:
	\bverbatim
		?- polynomial_prod(3*x + 2*y, 4*z - x, R).
		R = -2*x*y+12*x*z+8*y*z-3*x^2.
	\everbatim
	@constrs Both @Poly1 and @Poly2 are expanded multivariate polynomials.
*/
polynomial_prod(P1, P2, P):-
	list_from_polynomial(P1, L1), list_from_polynomial(P2, L2),
	polynomial_from_list_prod_list(L1, L2, MON_PROD),
	polynomial_from_list(MON_PROD, P).

%---------------
%%	 POWER

/*! Power of a polynomial */

/**
	@form polynomial_from_list_power_list(List, Value, PowerList)
	@descr @PowerList is the reduced list of monomials equal to raising
	to the power @Value the polynomial represented in @List.
	@constrs
		@param List A reduced list of monomials.
*/
polynomial_from_list_power_list(_, 0, [1]):- !.
polynomial_from_list_power_list(L, 1, L):- !.
polynomial_from_list_power_list(LP, N, LN):-
	natural(N), even(N),
	Nhalf is N/2,
	polynomial_from_list_power_list(LP, Nhalf, L),
	polynomial_from_list_prod_list(L, L, LN), !.
polynomial_from_list_power_list(LP, N, LN):-
	natural(N), odd(N),
	N1 is N - 1, NmHalf is N1/2,
	polynomial_from_list_power_list(LP, NmHalf, L),
	polynomial_from_list_prod_list(L, L, LN1),
	polynomial_from_list_prod_list(LN1, LP, LN), !.

/**
	@form polynomial_power(Poly, Value, PowerPoly)
	@descr @PowerPoly is the reduced polynomial equal to raising
	to the power @Value the polynomial @Poly.
	
	An example of usage is:
	\bverbatim
		?- polynomial_power(3*x + z, 2, R).
		R = 6*x*z+9*x^2+z^2.
	\everbatim
	@constrs @Poly is an expanded multivariate polynomial.
*/
polynomial_power(_, 0, 1):- !.
polynomial_power(P, 1, P):- !.
polynomial_power(P, N, PN):-
	list_from_polynomial(P, M),
	polynomial_from_list_power_list(M, N, L),
	polynomial_from_list(L, PN).


