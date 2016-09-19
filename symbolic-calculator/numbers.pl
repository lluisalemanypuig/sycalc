:-ensure_loaded(integer_algorithms).

% NATURALS

zero(0).
one(1).

natural(N):- integer(N), N >= 0.

% RATIONALS

% A fraction is the quotient of two ¡integer! numbers

numerator(A/_, A).
denominator(_/B, B).
fraction_comp(A, N, D):- numerator(A, N), denominator(A, D).

irred_frac(-(A/B)):- gcd(A, B, G), G is 1, !.
irred_frac(A/B):- A < 0, AA is -A, gcd(AA, B, G), G is 1, !.
irred_frac(A/B):- gcd(A, B, G), G is 1, !.

red_frac(0/_, 0):- !.
red_frac(A/0, A/0):- write('Warning (reduced_fraction): denominator is zero.'), nl, !.
red_frac(A/1, A):- !. 
red_frac(A/A, 1):- !. 
red_frac(A/B, C):- A < 0, AA is -A, gcd(AA, B, G), G is B, C is -AA/G, !.
red_frac(A/B, C/D):- A < 0, AA is -A, gcd(AA, B, G), C is -AA/G, D is B/G, !.
red_frac(A/B, C):- gcd(A, B, G), G is B, C is A/G, !.
red_frac(A/B, C/D):- gcd(A, B, G), C is A/G, D is B/G, !.

frac_sum(A/B, C/B, E/B):- E is A + C, !.
frac_sum(A/B, C/D, E/F):- F is B*D, E is A*D + B*C.

frac_sub(A/B, C/B, E/B):- E is A - C, !.
frac_sub(A/B, C/D, E/F):- F is B*D, E is A*D - B*C.

frac_prod(A/B, C/D, E/F):- E is A*C, F is B*D.
frac_div(A/B, C/D, E/F):- E is A*D, F is B*C.
frac_pow(A/B, C, E/F):- E is A^C, F is B^C.

frac_rev(A/B, B/A).

neg_frac(A/B, C/B):- B >= 0, C is -A, !.
neg_frac(A/B, A/D):- D is -B.

fraction_eval(A/B, C):- B \= 0, C is A/B.

% A fraction is a numerator divided by a denominator. It does not
% matter whether the numerator and/or denominator are not numbers
%*-- An integer x, although equal to x/1, is not a fraction --*
fraction(_/_).

% RATIONALS

rational(A):- integer(A), !.
rational(A):- fraction(A).

rational_neg(I, N):- integer(I), N is -I, !.
rational_neg(F, N):- neg_frac(F, N).

% IRRATIONALS

irrational(A):- not(fraction(A)), number(A).

% REALS

real(A):- rational(A), !.
real(A):- irrational(A).

abs_real(X, AX):- fraction(X), X < 0, neg_frac(X, AX), !.
abs_real(X, X):- fraction(X), X > 0, !.
abs_real(X, AX):- abs(X, AX).

% UTILS

max_num(X, Y, X):- X >= Y, !.
max_num(_, Y, Y).

min_num(X, Y, X):- X =< Y, !.
min_num(_, Y, Y).
