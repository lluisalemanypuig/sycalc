%% NUMERICAL FUNCTIONS

% NATURALS

zero(0).
one(1).

natural(N):- integer(N), N >= 0.

% multiple(A, B, X):- X = (A mod B == 0)
multiple(A, A):- true, !.
multiple(A, B):- D is A mod B, D == 0, !.
multiple(_, _):- false.

gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).

% INTEGRALS

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

fraction_neg(A/B, C/B):- C is -A.

eval_fraction(A/B, C):- B \= 0, C is A/B.

fraction(A):- numerator(A, N), denominator(A, D), integer(N), natural(D).

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
sum(A, B, C):- fraction(A), fraction(B), fraction_sum(A, B, R), reduced_fraction(R, C), !.
sum(A, B, C):- fraction(A), fraction_sum(A, B/1, R), reduced_fraction(R, C), !.
sum(A, B, C):- fraction(B), fraction_sum(A/1, B, R), reduced_fraction(R, C), !.
sum(A, B, C):- C is A + B.

% C is A - B
sub(A, B, C):- fraction(A), fraction(B), fraction_sub(A, B, R), reduced_fraction(R, C), !.
sub(A, B, C):- fraction(A), fraction_sub(A, B/1, R), reduced_fraction(R, C), !.
sub(A, B, C):- fraction(B), fraction_sub(A/1, B, R), reduced_fraction(R, C), !.
sub(A, B, C):- C is A - B.

% C is A*B
prod(A, B, C):- fraction(A), fraction(B), fraction_prod(A, B, R), reduced_fraction(R, C), !.
prod(A, B, C):- fraction(A), fraction_prod(A, B/1, R), reduced_fraction(R, C), !.
prod(A, B, C):- fraction(B), fraction_prod(A/1, B, R), reduced_fraction(R, C), !.
prod(A, B, C):- C is A*B.

% C is A/B
div(A, B, C):- prod(A, 1/B, C).

% C is A^B
pow(A, B, C):- fraction(A), fraction(B), fraction_pow(A, B, R), reduced_fraction(R, C), !.
pow(A, B, C):- fraction(A), fraction_pow(A, B/1, R), reduced_fraction(R, C), !.
pow(A, B, C):- fraction(B), fraction_pow(A/1, B, R), reduced_fraction(R, C), !.
pow(A, B, C):- C is A^B.

eval(A, A):- number(A), !.
eval(A + B, C):- eval(A, AA), eval(B, BB), sum(AA, BB, C), !.
eval(A - B, C):- eval(A, AA), eval(B, BB), sub(AA, BB, C), !.
eval(A*B, C):- eval(A, AA), eval(B, BB), prod(AA, BB, C), !.
eval(A/B, C):- eval(A, AA), eval(B, BB), reduced_fraction(AA/BB, C), !.
eval(A^B, C):- eval_pow(A^B, C), !.
eval(A^B, C):- eval(A, AA), eval(B, BB), pow(AA, BB, C).

eval_pow(A^B^C, R):- eval(A^B, R1), eval(R1^C, R).

% COMPARISONS

eq(X, Y):- eval(X, XX), eval(Y, YY), XX == YY.
lt(X, Y):- eval(X, XX), eval(Y, YY), XX <  YY.
le(X, Y):- eval(X, XX), eval(Y, YY), XX =< YY.
gt(X, Y):- eval(X, XX), eval(Y, YY), XX >  YY.
ge(X, Y):- eval(X, XX), eval(Y, YY), XX >= YY.

% EXPRESSIONS

expr(_ + _).
expr(_ - _).
expr(_*_).
expr(_/_).
expr(_^_).



%%%%%%%%%%%%
% ----------
% DEBUG

red_frac(F, TAB, RES):- reduced_fraction(F, R), write(F), write(' = '), write(R), write(TAB), write(' | correct? '), R == RES, !, write('Yes'), nl.
red_frac(_,   _,   _):- write('No'), nl, false.

sum_deb(A, B, TAB, RES):- sum(A, B, R), write(A), write(' + '), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
sum_deb(_, _,   _,   _):- write('No'), nl, false.

sub_deb(A, B, TAB, RES):- sub(A, B, R), write(A), write(' - '), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
sub_deb(_, _,   _,   _):- write('No'), nl, false.

prod_deb(A, B, TAB, RES):- prod(A, B, R), write(A), write('*'), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
prod_deb(_, _,   _,   _):- write('No'), nl, false.

debug_numeric:-
	write('- REDUCED FRACTIONS -'), nl, nl,

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
	
	true.