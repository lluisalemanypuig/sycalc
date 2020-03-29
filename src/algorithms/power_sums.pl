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

:-ensure_loaded("../core").

/***
	@descr This file contains the algorithms to compute the polynomial
	that represents the sum of the first n natural numbers each raised
	to a certain power.
	
	This power is given in the predicate and must be a natural value.
*/

% FORMULA OF SUMS OF POWERS

h_coefficient(C, H):-
	frac_sum(C, 1/1, R),
	fraction_comp(R, N, D),
	fraction_comp(H, D, N).

reminder_(_, [], []):- !.
reminder_([[D, F]|_], [M], R):-
	unimonomial_degree(M, D),
	monomial_coefficient(M, MC),
	map(mon_prod(MC), F, R), !.
reminder_([_|Fs], Ms, R):- reminder_(Fs, Ms, R), !.
reminder_([[D, F]|Fs], [M|Ms], R):-
	unimonomial_degree(M, D),
	monomial_coefficient(M, MC),
	map(mon_prod(MC), F, R1),
	reminder_(Fs, Ms, R2),
	polynomial_from_list_sum_list(R1, R2, R), !.
reminder_([_|Fs], Ms, R):- reminder_(Fs, Ms, R).

reminder(F, P, R):- reminder_(F, P, R).

power_sums_(0, [n], [[0, n]]):- !.
power_sums_(1, [(1/2)*n^2 ,(1/2)*n], [[0, n], [1, [(1/2)*n^2, (1/2)*n]]]):- !.
power_sums_(D, SUM, L):-
	D1 is D - 1,
	power_sums_(D1, S1, L1),

	first(S1, S1F, S1R),

	% H = 1/(1 + c_{d + 1})
	monomial_coefficient(S1F, FH), h_coefficient(FH, H),

	first(S1R, S1RF, S1RR),

	monomial_coefficient(S1RF, COEF),

	% B = (n + 1 - c_d)
	polynomial_expression_evaluation((n + 1 - COEF), B),

	list_from_polynomial(B, BMS),

	% S = B*p(n, d)
	unipoly_from_list_prod_sorted_list(BMS, S1, S),

	% R = sum[j=1->d] c_{d - j}*p(n, d - j)
	first(L1, _, L1R), reminder(L1R, S1RR, R),

	unipoly_from_list_sub_sorted_list(S, R, S_MINUS_R),

	% SUM = H*(S - R)
	unipoly_from_list_prod_sorted_list([H], S_MINUS_R, SUM),

	list_concat([[D,SUM]], L1, L),
	!.

/**
	@form power_sums(Power, Poly)
	@descr @Poly is a univariate polynomial on variable n that computes
	the value:
	\bverbatim
	\sum_{i=1}^n i^Power
	\everbatim
	@constrs
		@param Power A natural value.
*/
power_sums(D, S):- power_sums_(D, SS, _), polynomial_from_list(SS, S).

% L = [i, f(n, i)] where f(n, i) = 1^i + 2^i + ... + n^i
/**
	@form power_sums_list(Power, ListPoly)
	@descr The j-th element of @ListPoly is a list of two values:
	\blist
	\item A natural value P
	\item A univariate polynomial in variable n that computes the sum
	\bverbatim
	\sum_{i=1}^n i^P
	\everbatim
	++>
	@ListPoly is decreasingly sorted, that is, the first value P in @ListPoly
	for @Power is @Power itself.
	@constrs
		@param Power A natural value.
*/
power_sums_list(D, L):- power_sums_(D, _, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Computes iteratively the sum: 1^D + 2^D + ... + N^D
sum_from_1_to_n_to_D(1, _, 1):- !.
sum_from_1_to_n_to_D(N, D, S):-
	N1 is N - 1, sum_from_1_to_n_to_D(N1, D, S1),
	S is S1 + N^D.

%D is 3, power_sums(D, S1), power_sums(D, S2).
