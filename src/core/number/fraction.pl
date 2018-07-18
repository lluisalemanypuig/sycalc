:-ensure_loaded(integer).

/***
	@descr This file contains rational-related predicates. The definition
	of rational value (the quotient of an integer value and a natural
	value), reduced fractions, ...
*/

/**
	@form fraction(A/B)
	@constrs A is an integer value, B is a strictly positive natural
	value (> 0).
	@descr A fraction is the quotient of an integer number and a natural
	number. Any integer value X, although being equal to X/1, is not a
	fraction.
*/
fraction(A/B):- integer(A), natural(B), B > 0.

/**
	@form numerator(A/B, A).
	@descr The numerator of a fraction A/B is the value A.
*/
numerator(N/_, N).

/**
	@form denominator(A/B, B).
	@descr The denominator of a fraction A/B is the value B.
*/
denominator(_/D, D).

/**
	@form fraction_comp(A/B, A, B)
	@descr The components of a fraction A/B are the numerator A and
	the denominator B.
*/
fraction_comp(F, N, D):- numerator(F, N), denominator(F, D).

/**
	@form irred_frac(A/B)
	@constrs A/B is a fraction (predicate fraction(A/B) does not fail)
	@descr Predicate fails if A/B can be reduced, that is, if the greatest
	common divisor between A and B is not 1.
*/
irred_frac(-(A/B)):- gcd(A, B, 1), !.
irred_frac(A/B):- A < 0, AA is -A, gcd(AA, B, 1), !.
irred_frac(A/B):- gcd(A, B, 1), !.

/**
	@form red_frac(A/B, C)
	@constrs A/B is a fraction (predicate fraction(A/B) does not fail)
	@descr C is either an integer value or an irreducible fraction equal
	to A/B.
*/
red_frac(0/_, 0):- !.
red_frac(A/1, A):- !. 
red_frac(A/A, 1):- !. 
red_frac(A/B, C):- A < 0, AA is -A, gcd(AA, B, G), G is B, C is -AA/G, !.
red_frac(A/B, C/D):- A < 0, AA is -A, gcd(AA, B, G), C is -AA/G, D is B/G, !.
red_frac(A/B, C):- gcd(A, B, G), G is B, C is A/G, !.
red_frac(A/B, C/D):- gcd(A, B, G), C is A/G, D is B/G, !.

/**
	@form frac_sum(A/B, C/D, E/F)
	@constrs A/B and C/D are fractions (predicates fraction(A/B) and
	fraction(C/D) do not fail)
	@descr E/F is a fraction equal to A/B + C/D. May be reducible.
*/
frac_sum(A/B, C/B, E/B):- E is A + C, !.
frac_sum(A/B, C/D, E/F):- F is B*D, E is A*D + B*C.

/**
	@form frac_sub(A/B, C/D, E/F)
	@constrs A/B and C/D are fractions (predicates fraction(A/B) and
	fraction(C/D) do not fail)
	@descr E/F is a fraction equal to A/B - C/D. May be reducible.
*/
frac_sub(A/B, C/B, E/B):- E is A - C, !.
frac_sub(A/B, C/D, E/F):- F is B*D, E is A*D - B*C.

/**
	@form frac_prod(A/B, C/D, E/F)
	@constrs A/B and C/D are fractions (predicates fraction(A/B) and
	fraction(C/D) do not fail)
	@descr E/F is a fraction equal to (A/B)*(C/D). May be reducible.
*/
frac_prod(A/B, C/D, E/F):- E is A*C, F is B*D.

/**
	@form frac_div(A/B, C/D, E/F)
	@constrs A/B and C/D are fractions (predicates fraction(A/B) and
	fraction(C/D) do not fail)
	@descr E/F is a fraction equal to (A/B)/(C/D). May be reducible.
*/
frac_div(A/B, C/D, E/F):- E is A*D, F is B*C.

/**
	@form frac_div(A/B, C, E/F)
	@constrs A/B is a fraction (predicate fraction(A/B) does not fail)
	and C is a natural value.
	@descr E/F is a fraction equal to (A/B)^C. May be reducible.
*/
frac_pow(A/B, C, E/F):- E is A^C, F is B^C.

/**
	@form frac_inv(A/B, C/D)
	@constrs A/B is a fraction (predicate fraction(A/B) does not fail).
	@descr C/D is a fraction equal to 1/(A/B).
*/
frac_inv(A/B, BB/AA):- A < 0, BB is -B, AA is -A, !.
frac_inv(A/B, B/A).

/**
	@form neg_frac(A/B, C/D)
	@constrs A/B is a fraction (predicate fraction(A/B) does not fail).
	@descr C/D is a fraction equal to -(A/B).
*/
neg_frac(A/B, C/B):- C is -A, !.

/**
	@form fraction_eval(A/B, C)
	@constrs A/B is a fraction (predicate fraction(A/B) does not fail).
	@descr C is a value equal to the result of dividing A by B.
*/
fraction_eval(A/B, C):- C is A/B.

/**
	@form frac_gcd(A/B, C/D, F/G)
	@constrs A/B and C/D are fractions (predicates fraction(A/B) and
	fraction(C/D) do not fail) and irreducible.
	@descr F is the greatest common divisor and A and C; and G is the
	greatest common divisor of B and D.
*/
frac_gcd(A/B, C/D, FF/GG):- gcd(A, C, FF), gcd(B, D, GG).

/**
	@form frac_gcd_rel(A/B, C/D, F/G, AA/BB, CC/DD)
	@constrs A/B and C/D are fractions (predicates fraction(A/B) and
	fraction(C/D) do not fail) and irreducible.
	@descr F is the greatest common divisor and A and C; and G is the
	greatest common divisor of B and D.
*/
frac_gcd_rel(A/B, C/D, FF/GG, AA/BB, CC/DD):-
	frac_gcd(A/B, C/D, FF/GG),
	AA is A/FF, CC is C/FF, BB is B/GG, DD is D/GG.


