:-include(numerical_algorithms).

% NATURALS

zero(0).
one(1).

natural(N):- integer(N), N >= 0.

% FRACTIONS

% A fraction is the quotient of two ¡integer! numbers

numerator(A/_, A).
denominator(_/B, B).
fraction_components(A, N, D):- numerator(A, N), denominator(A, D).

irreducible_fraction(-(A/B)):- gcd(A, B, G), G is 1, !.
irreducible_fraction(A/B):- A < 0, AA is -A, gcd(AA, B, G), G is 1, !.
irreducible_fraction(A/B):- gcd(A, B, G), G is 1, !.

reduced_fraction(0/_, 0):- !.
reduced_fraction(A/0, A/0):- write('Warning (reduced_fraction): denominator is zero.'), nl, !.
reduced_fraction(A/1, A):- !. 
reduced_fraction(A/A, 1):- !. 
reduced_fraction(A/B, C):- A < 0, AA is -A, gcd(AA, B, G), G is B, C is -AA/G, !.
reduced_fraction(A/B, C/D):- A < 0, AA is -A, gcd(AA, B, G), C is -AA/G, D is B/G, !.
reduced_fraction(A/B, C):- gcd(A, B, G), G is B, C is A/G, !.
reduced_fraction(A/B, C/D):- gcd(A, B, G), C is A/G, D is B/G, !.

fraction_sum(A/B, C/B, E/B):- E is A + C, !.
fraction_sum(A/B, C/D, E/F):- F is B*D, E is A*D + B*C.

fraction_sub(A/B, C/B, E/B):- E is A - C, !.
fraction_sub(A/B, C/D, E/F):- F is B*D, E is A*D - B*C.

fraction_prod(A/B, C/D, E/F):- E is A*C, F is B*D.
fraction_div(A/B, C/D, E/F):- E is A*D, F is B*C.
fraction_pow(A/B, C, E/F):- E is A^C, F is B^C.

fraction_rev(A/B, B/A).

neg_fraction(A/B, C/B):- B >= 0, C is -A, !.
neg_fraction(A/B, A/D):- D is -B.

eval_fraction(A/B, C):- B \= 0, C is A/B.

% A fraction is a numerator divided by a denominator. It does not
% matter whether the numerator and/or denominator are not numerals
%*-- An integer x, although equal to x/1, is not a fraction --*
fraction(_/_).

% RATIONALS

rational(A):- integer(A), !.
rational(A):- fraction(A).

% IRRATIONALS

irrational(A):- not(rational(A)), number(A).

% REALS

real(A):- rational(A), !.
real(A):- irrational(A).

% ARITHMETIC EXPRESSIONS' EVALUATION

% C is A + B
eval_sum(A, B, C):- fraction(A), fraction(B), fraction_sum(A, B, R), reduced_fraction(R, C), !.
eval_sum(A, B, C):- fraction(A), fraction_sum(A, B/1, R), reduced_fraction(R, C), !.
eval_sum(A, B, C):- fraction(B), fraction_sum(A/1, B, R), reduced_fraction(R, C), !.
eval_sum(A, B, C):- C is A + B.

% C is A - B
eval_sub(A, B, C):- fraction(A), fraction(B), fraction_sub(A, B, R), reduced_fraction(R, C), !.
eval_sub(A, B, C):- fraction(A), fraction_sub(A, B/1, R), reduced_fraction(R, C), !.
eval_sub(A, B, C):- fraction(B), fraction_sub(A/1, B, R), reduced_fraction(R, C), !.
eval_sub(A, B, C):- C is A - B.

% C is A*B
eval_prod(A, B, C):- fraction(A), fraction(B), fraction_prod(A, B, R), reduced_fraction(R, C), !.
eval_prod(A, B, C):- fraction(A), fraction_prod(A, B/1, R), reduced_fraction(R, C), !.
eval_prod(A, B, C):- fraction(B), fraction_prod(A/1, B, R), reduced_fraction(R, C), !.
eval_prod(A, B, C):- C is A*B.

% C is A/B
eval_div(A, B, C):- fraction(A), fraction(B), fraction_div(A, B, R), reduced_fraction(R, C), !.
eval_div(A, B, C):- fraction(A), fraction_div(A, B/1, R), reduced_fraction(R, C), !.
eval_div(A, B, C):- fraction(B), fraction_div(A/1, B, R), reduced_fraction(R, C), !.
eval_div(A, B, C):- multiple(A, B), C is A/B.
eval_div(A, B, A/B).

% C is A^B
eval_pow(A, B, C):- fraction(A), fraction(B), fraction_pow(A, B, R), reduced_fraction(R, C), !.
eval_pow(A, B, C):- fraction(A), fraction_pow(A, B/1, R), reduced_fraction(R, C), !.
eval_pow(A, B, C):- fraction(B), fraction_pow(A/1, B, R), reduced_fraction(R, C), !.
eval_pow(A, B, C):- C is A^B.

% ARITHMETIC EVALUATION

% An arithmetic expression is a sum, sub, prod, div or pow of real numbers
arithmetic_eval(A + B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_sum(AA, BB, C), !.
arithmetic_eval(A - B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_sub(AA, BB, C), !.
arithmetic_eval(A*B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_prod(AA, BB, C), !.
arithmetic_eval(A/B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), reduced_fraction(AA/BB, C), !.
arithmetic_eval(A^B, C):- arithmetic_eval_pow(A^B, C), !.
arithmetic_eval(A^B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_pow(AA, BB, C), !.
arithmetic_eval(A, A):- real(A).

arithmetic_eval_pow(A^B^C, R):- arithmetic_eval(A^B, R1), arithmetic_eval(R1^C, R).

% EXPRESSIONS

expr(_ + _).
expr(_ - _).
expr(_*_).
expr(_/_).
expr(_^_).

/*
-----------------------------
DEBUG
*/

red_frac(F, TAB, RES):- reduced_fraction(F, R), write(F), write(' = '), write(R), write(TAB), write(' | correct? '), R == RES, !, write('Yes'), nl.
red_frac(_,   _,   _):- write('No'), nl, false.

sum_deb(A, B, TAB, RES):- eval_sum(A, B, R), write(A), write(' + '), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
sum_deb(_, _,   _,   _):- write('No'), nl, false.

sub_deb(A, B, TAB, RES):- eval_sub(A, B, R), write(A), write(' - '), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
sub_deb(_, _,   _,   _):- write('No'), nl, false.

prod_deb(A, B, TAB, RES):- eval_prod(A, B, R), write(A), write('*'), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
prod_deb(_, _,   _,   _):- write('No'), nl, false.

power_deb(A, B, TAB, RES):- eval_pow(A, B, R), write(A), write('^'), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
power_deb(_, _,   _,   _):- write('No'), nl, false.

arithm_deb(E, TAB, RES):- arithmetic_eval(E, R), write(E), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
arithm_deb(_,   _,   _):- write('No'), nl, false.

debug_numeric:-
	nl, write('-- NUMERICAL DEBUG --'), nl,
	nl, write('- REDUCED FRACTIONS -'), nl, nl,

	write(' 1) '), red_frac(0/1, '     ', 0),
	write(' 2) '), red_frac(0/5, '     ', 0),
	write(' 3) '), red_frac(1/2, '   ', 1/2),
	write(' 4) '), red_frac(4/2, '     ', 2),
	write(' 5) '), red_frac(5/2, '   ', 5/2),
	write(' 6) '), red_frac(6/2, '     ', 3),
	write(' 7) '), red_frac(6/4, '   ', 3/2),
	write(' 8) '), red_frac(6/8, '   ', 3/4),
	write(' 9) '), red_frac(6/10, '  ', 3/5),
	write('10) '), red_frac(-0/1, '     ', 0),
	write('11) '), red_frac(-0/5, '     ', 0),
	write('12) '), red_frac(-1/2, ' ', -1/2),
	write('13) '), red_frac(-4/2, '   ', -2),
	write('14) '), red_frac(-5/2, ' ', -5/2),
	write('15) '), red_frac(-6/2, '   ', -3),
	write('16) '), red_frac(-6/4, ' ', -3/2),
	write('17) '), red_frac(-6/8, ' ', -3/4),
	write('18) '), red_frac(-6/10, '', -3/5),
	
	nl, write('- SUMS -'), nl, nl,

	write(' 1) '), sum_deb(0, 0, '       ', 0),
	write(' 2) '), sum_deb(1, 0, '       ', 1),
	write(' 3) '), sum_deb(4, 0, '       ', 4),
	write(' 4) '), sum_deb(0, 1, '       ', 1),
	write(' 5) '), sum_deb(0, 5, '       ', 5),
	write(' 6) '), sum_deb(1/2, 1/2, '   ', 1),
	write(' 7) '), sum_deb(1/2, 2/2, ' ', 3/2),
	write(' 8) '), sum_deb(1/2, 1, '   ', 3/2),
	write(' 9) '), sum_deb(1, 1/2, '   ', 3/2),
	write('10) '), sum_deb(2/2, 2/2, '   ', 2),
	write('11) '), sum_deb(2/2, 3/2, ' ', 5/2),
	write('12) '), sum_deb(3/2, 1, '   ', 5/2),
	
	nl, write('- SUBS -'), nl, nl,

	write(' 1) '), sub_deb(0, 0, '       ', 0),
	write(' 2) '), sub_deb(1, 0, '       ', 1),
	write(' 3) '), sub_deb(4, 0, '       ', 4),
	write(' 4) '), sub_deb(0, 1, '      ', -1),
	write(' 5) '), sub_deb(0, 5, '      ', -5),
	write(' 6) '), sub_deb(1/2, 1/2, '   ', 0),
	write(' 7) '), sub_deb(1/2, 2/2, '', -1/2),
	write(' 8) '), sub_deb(1/2, 1, '  ', -1/2),
	write(' 9) '), sub_deb(1, 1/2, '   ', 1/2),
	write('10) '), sub_deb(2/2, 2/2, '   ', 0),
	write('11) '), sub_deb(2/2, 3/2, '', -1/2),
	write('12) '), sub_deb(3/2, 1, '   ', 1/2),
	
	nl, write('- PRODS -'), nl, nl,

	write(' 1) '), prod_deb(0, 0, '      ', 0),
	write(' 2) '), prod_deb(1, 0, '      ', 0),
	write(' 3) '), prod_deb(4, 0, '      ', 0),
	write(' 4) '), prod_deb(0, 1, '      ', 0),
	write(' 5) '), prod_deb(0, 5, '      ', 0),
	write(' 6) '), prod_deb(1/2, 1/2, '', 1/4),
	write(' 7) '), prod_deb(1/2, 2/2, '', 1/2),
	write(' 8) '), prod_deb(1/2, 1, '  ', 1/2),
	write(' 9) '), prod_deb(1, 1/2, '  ', 1/2),
	write('10) '), prod_deb(2/2, 2/2, '  ', 1),
	write('11) '), prod_deb(2/2, 3/2, '', 3/2),
	write('12) '), prod_deb(3/2, 1, '  ', 3/2),
	
	nl, write('- POWERS -'), nl, nl,

	write(' 1) '), power_deb(0, 0, '    ', 1),
	write(' 2) '), power_deb(1, 0, '    ', 1),
	write(' 3) '), power_deb(4, 0, '    ', 1),
	write(' 4) '), power_deb(0, 1, '    ', 0),
	write(' 5) '), power_deb(0, 5, '    ', 0),
	write(' 6) '), power_deb(1/2, 2, '', 1/4),
	write(' 7) '), power_deb(1/2, 1, '', 1/2),
	write(' 8) '), power_deb(2, 3, '    ', 8),
	write(' 9) '), power_deb(2/2, 2, '  ', 1),
	write('10) '), power_deb(2/2, 3, '  ', 1),
	write('11) '), power_deb(3/2, 1, '', 3/2),
	write('12) '), power_deb(3/2, 2, '', 9/4),
	
	nl, write('- ARITHMETIC EXPRESSIONS -'), nl, nl,
	write(' 1) '), arithm_deb(3 + 3, '                 ', 6),
	write(' 2) '), arithm_deb(3 - 4, '                ', -1),
	write(' 3) '), arithm_deb(3^4, '                ', 81),
	write(' 4) '), arithm_deb(2^2^2^2, '           ', 256),
	write(' 5) '), arithm_deb(2^2^2^2^2, '       ', 65536),
	write(' 6) '), arithm_deb(2^(1 + 1)^2, '         ', 16),
	write(' 7) '), arithm_deb(2^2^(2 + 1 - 1)^2^2, '', 65536),
	write(' 8) '), arithm_deb((1/2)^2 + 3/4, '         ', 1),
	write(' 9) '), arithm_deb((1/2)^0, '             ', 1),
	write('10) '), arithm_deb((1/2)^0 + 3/4, '       ', 7/4),
	write('11) '), arithm_deb(1 - (1 + 1), '           ', -1),
	write('12) '), arithm_deb(1/2, '               ', 1/2),
	true.