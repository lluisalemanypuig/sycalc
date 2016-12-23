:-ensure_loaded(core).

% FORMULA OF SUMS OF POWERS

h_coefficient(C, H):- frac_sum(C, 1/1, R), fraction_comp(R, N, D), fraction_comp(H, D, N).

reminder_(_, [], 0):- !.
reminder_([[D, F]|_], [M], R):-
	monomial_degree(M, D), monomial_coefficient(M, MC),
	list_polynomial_prod_list([MC], F, R), !.
reminder_([_|Fs], Ms, R):- reminder_(Fs, Ms, R), !.
reminder_([[D, F]|Fs], [M|Ms], R):-
	monomial_degree(M, D), monomial_coefficient(M, MC),
	list_polynomial_prod_list([MC], F, R1),
	reminder_(Fs, Ms, R2),
	list_polynomial_sum_list(R1, R2, R), !.
reminder_([_|Fs], Ms, R):- reminder_(Fs, Ms, R).

reminder(F, P, R):- reminder_(F, P, R).

power_sums_(1, [(1/2)*n^2 ,(1/2)*n], [[1, [(1/2)*n^2, (1/2)*n]]]):- !.
power_sums_(D, S, L):- 
	D1 is D - 1,
	power_sums_(D1, S1, L1),
	list_polynomial_prod_list([n,1], S1, T),
	first(S1, F, R),
	monomial_coefficient(F, FH), h_coefficient(FH, H),
	reminder(L1, R, REM),
	list_polynomial_sub_list(T, REM, T_MINUS_REM),
	list_polynomial_prod_list([H], T_MINUS_REM, S),
	concat([[D,S]], L1, L),
	!.

% S = f(n) = 1^D + 2^D + ... + n^D
power_sums(D, S):- power_sums_(D, SS, _), list_polynomial(SS, S).

% L = [i, f(n, i)] where f(n, i) = 1^i + 2^i + ... + n^i
power_sums_list(D, L):- power_sums_(D, _, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Computes iteratively the sum: 1^D + 2^D + ... + N^D
sum_from_1_to_n_to_D(1, _, 1):- !.
sum_from_1_to_n_to_D(N, D, S):-
	N1 is N - 1, sum_from_1_to_n_to_D(N1, D, S1),
	S is S1 + N^D.

%D is 3, power_sums(D, S1), power_sums2(D, S2).