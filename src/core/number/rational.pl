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

:-ensure_loaded(natural).
:-ensure_loaded(integer).
:-ensure_loaded(fraction).

% RATIONALS

/**
	@form rational(R)
	@descr A number @R is a rational number if it is either an integer
	number or a fraction.
*/
rational(R):- integer(R), !.
rational(R):- fraction(R).

/**
	@form rational_neg(R, N)
	@descr @N is equal to -@R.
*/
rational_neg(I, N):- integer(I), N is -I, !.
rational_neg(F, N):- neg_frac(F, N).

/**
	@form rational_gcd(A,B, G)
	@descr @G is the greatest common divisor of @A and @B.
	@constrs @A and @B are both rational numbers.
*/
rational_gcd(A, B, G):- integer(A), integer(B), gcd(A, B, G), !.
rational_gcd(A, B, G):- integer(A), frac_gcd(A/1, B, GG), red_frac(GG, G), !.
rational_gcd(A, B, G):- integer(B), frac_gcd(A, B/1, GG), red_frac(GG, G), !.
rational_gcd(A, B, G):- frac_gcd(A, B, GG), red_frac(GG, G).

/**
	@form rational_gcd_rel(A,B, G, C,D)
	@descr @G is the greatest common divisor of @A and @B. @C and @D are
	the result of
	\bverbatim
		gcd_rel(A,B, G, C,D)
		frac_gcd_rel(A,B, G, C,D)
	\everbatim
	@constrs @A and @B are both rational numbers.
*/
rational_gcd_rel(A, B, G, C, D):-
	integer(A), integer(B), gcd_rel(A, B, G, C, D), !.
rational_gcd_rel(A, B, G, C, D):-
	integer(A), frac_gcd_rel(A/1, B, GG, CC, DD), red_frac(GG, G),
	red_frac(CC, C), red_frac(DD, D), !.
rational_gcd_rel(A, B, G, C, D):-
	integer(B), frac_gcd_rel(A, B/1, GG, CC, DD), red_frac(GG, G),
	red_frac(CC, C), red_frac(DD, D), !.
rational_gcd_rel(A, B, G, C, D):-
	frac_gcd_rel(A, B, GG, CC, DD), red_frac(GG, G), red_frac(CC, C),
	red_frac(DD, D).
