:-ensure_loaded(integer).

% A fraction is the quotient of two ¡integer! numbers

numerator(N/_, N).
denominator(_/D, D).
fraction_comp(F, N, D):- numerator(F, N), denominator(F, D).

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

% A/B, C/D, FF/GG : irreducible fractions
% A/B + C/D = (FF/GG)*(AA/BB + CC/DD)
% A/B - C/D = (FF/GG)*(AA/BB - CC/DD)
% A/B * C/D = (FF/GG)*(AA/BB * CC/DD)
% A/B / C/D = (FF/GG)*(AA/BB / CC/DD)
% for some AA/BB AND CC/DD irreducible fractions
frac_gcd(A/B, C/D, FF/GG):- gcd(A, C, FF), gcd(B, D, GG).

% A/B, C/D, FF/GG, AA/BB, CC/DD : irreducible fractions
% A/B + C/D = (FF/GG)*(AA/BB + CC/DD)
% A/B - C/D = (FF/GG)*(AA/BB - CC/DD)
% A/B * C/D = (FF/GG)*(AA/BB * CC/DD)
% A/B / C/D = (FF/GG)*(AA/BB / CC/DD)
frac_gcd_rel(A/B, C/D, FF/GG, AA/BB, CC/DD):-
	frac_gcd(A/B, C/D, FF/GG),
	AA is A/FF, CC is C/FF, BB is B/GG, DD is D/GG.

% A fraction is a numerator divided by a denominator. It does not
% matter whether the numerator and/or denominator are not numbers
%*-- An integer x, although equal to x/1, is not a fraction --*
fraction(_/_).

