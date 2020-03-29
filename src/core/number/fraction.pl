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

:-ensure_loaded(integer).

/***
	@descr This file contains rational-related predicates. The definition
	of rational value (the quotient of an integer value and a natural
	value), reduced fractions, ...
	
	Henceforth, it will be assumed that any fraction F is of the form
	F, where both A and B are ground.
*/

/**
	@form fraction(F).
	@descr A fraction is the quotient of an integer number and a natural
	number. Any integer value X, although being equal to X/1, is not a
	fraction.
*/
fraction(A/B):- integer(A), natural(B), B > 0.

/**
	@form numerator(F, A).
	@descr The numerator of a fraction @F is the value @A.
*/
numerator(N/_, N).

/**
	@form denominator(F, B).
	@descr The denominator of a fraction @F is the value @B.
*/
denominator(_/D, D).

/**
	@form fraction_comp(F, A, B)
	@descr The components of a fraction @F are the numerator @A and
	the denominator @B.
*/
fraction_comp(F, N, D):- numerator(F, N), denominator(F, D).

/**
	@form irred_frac(F)
	@descr Predicate fails if @F can be reduced, that is, if the
	greatest common divisor between its numerator and denominator is not 1.
*/
irred_frac(-(A/B)):- gcd(A, B, 1), !.
irred_frac(A/B):- A < 0, AA is -A, gcd(AA, B, 1), !.
irred_frac(A/B):- gcd(A, B, 1), !.

/**
	@form red_frac(F, C)
	@descr @C is either an integer value or an irreducible fraction equal
	to @F.
*/
red_frac(0/_, 0):- !.
red_frac(A/1, A):- !. 
red_frac(A/A, 1):- !. 
red_frac(A/B, C):- A < 0, AA is -A, gcd(AA, B, G), G is B, C is -AA/G, !.
red_frac(A/B, C/D):- A < 0, AA is -A, gcd(AA, B, G), C is -AA/G, D is B/G, !.
red_frac(A/B, C):- gcd(A, B, G), G is B, C is A/G, !.
red_frac(A/B, C/D):- gcd(A, B, G), C is A/G, D is B/G, !.

/**
	@form frac_sum(F1, F2, F3)
	@descr @F3 is a fraction equal to @F1 + @F2. May be reducible.
	@constrs @F1 and @F2 are fractions.
*/
frac_sum(A/B, C/B, E/B):- E is A + C, !.
frac_sum(A/B, C/D, E/F):- F is B*D, E is A*D + B*C.

/**
	@form frac_sum(F1, F2, F3)
	@descr @F3 is a fraction equal to @F1 - @F2. May be reducible.
	@constrs @F and @F2 are fractions.
*/
frac_sub(A/B, C/B, E/B):- E is A - C, !.
frac_sub(A/B, C/D, E/F):- F is B*D, E is A*D - B*C.

/**
	@form frac_sum(F1, F2, F3)
	@descr @F3 is a fraction equal to @F1*@F2. May be reducible.
	@constrs @F1 and @F2 are fractions.
*/
frac_prod(A/B, C/D, E/F):- E is A*C, F is B*D.

/**
	@form frac_sum(F1, F2, F3)
	@descr @F3 is a fraction equal to @F1/@F2. May be reducible.
	@constrs @F and @F2 are fractions.
*/
frac_div(A/B, C/D, E/F):- E is A*D, F is B*C.

/**
	@form frac_div(F1, C, F2)
	@descr @F2 is a fraction equal to @F1^@C. May be reducible.
	@constrs @F1 is a fraction and @C is a natural value.
*/
frac_pow(A/B, C, E/F):- E is A^C, F is B^C.

/**
	@form frac_inv(F1, F2)
	@descr @F2 is a fraction equal to 1/@F1.
	@constrs @F1 is a fraction.
*/
frac_inv(A/B, BB/AA):- A < 0, BB is -B, AA is -A, !.
frac_inv(A/B, B/A).

/**
	@form neg_frac(F1, F2)
	@descr @F2 is a fraction equal to -@F1.
	@constrs @F1 is a fraction.
*/
neg_frac(A/B, C/B):- C is -A, !.

/**
	@form fraction_eval(F, C)
	@descr @C is a value equal to the result of evaluating @F.
	@constrs @F is a fraction.
*/
fraction_eval(F, C):- C is F.

/**
	@form frac_gcd(F1, F2, F3)
	@descr Let @F1=A/B, @F2=C/D, @F3=F/G.
	
	F is the greatest common divisor
	and A and C; and G is the greatest common divisor of B and D.
	@constrs @F1 and @F2 are irreducible fractions.
*/
frac_gcd(A/B, C/D, FF/GG):- gcd(A, C, FF), gcd(B, D, GG).

/**
	@form frac_gcd_rel(F1, F2, F3, F4, F5)
	@descr Let @F1=A/B, @F2=C/D, @F3=F/G, @F4=AA/BB, @F5=CC/DD.
	
	F is the greatest common divisor and A and C; and G is the
	greatest common divisor of B and D.
	@constrs @1F and @F2 are irreducible fractions.
*/
frac_gcd_rel(A/B, C/D, FF/GG, AA/BB, CC/DD):-
	frac_gcd(A/B, C/D, FF/GG),
	AA is A/FF, CC is C/FF, BB is B/GG, DD is D/GG.


