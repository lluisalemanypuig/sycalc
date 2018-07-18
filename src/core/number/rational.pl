:-ensure_loaded(natural).
:-ensure_loaded(integer).
:-ensure_loaded(fraction).

% RATIONALS

/**
	@form rational(R)
	@descr A number R is a rational number if it is either an integer
	number or a fraction.
*/
rational(R):- integer(R), !.
rational(R):- fraction(R).

/**
	@form rational_neg(R, N)
	@descr N is equal to -R.
*/
rational_neg(I, N):- integer(I), N is -I, !.
rational_neg(F, N):- neg_frac(F, N).

/**
	@form rational_gcd(A,B, G)
	@constrs A and B are both rational numbers
	@descr G is the greatest common divisor of A and B.
*/
rational_gcd(A, B, G):- integer(A), integer(B), gcd(A, B, G), !.
rational_gcd(A, B, G):- integer(A), frac_gcd(A/1, B, GG), red_frac(GG, G), !.
rational_gcd(A, B, G):- integer(B), frac_gcd(A, B/1, GG), red_frac(GG, G), !.
rational_gcd(A, B, G):- frac_gcd(A, B, GG), red_frac(GG, G).

/**
	@form rational_gcd_rel(A,B, G, C,D)
	@constrs A and B are both rational numbers
	@descr G is the greatest common divisor of A and B. C and D are
	the result of
		gcd_rel(A,B, G, C,D)
		frac_gcd_rel(A,B, G, C,D)
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
