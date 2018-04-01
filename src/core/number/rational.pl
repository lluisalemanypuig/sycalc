:-ensure_loaded(natural).
:-ensure_loaded(integer).
:-ensure_loaded(fraction).

% RATIONALS

rational(R):- integer(R), !.
rational(R):- fraction(R).

rational_neg(I, N):- integer(I), N is -I, !.
rational_neg(F, N):- neg_frac(F, N).

rational_gcd(A, B, G):- integer(A), integer(B), gcd(A, B, G), !.
rational_gcd(A, B, G):- integer(A), frac_gcd(A/1, B, GG), red_frac(GG, G), !.
rational_gcd(A, B, G):- integer(B), frac_gcd(A, B/1, GG), red_frac(GG, G), !.
rational_gcd(A, B, G):- frac_gcd(A, B, GG), red_frac(GG, G).

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
