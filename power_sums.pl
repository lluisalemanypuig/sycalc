:-ensure_loaded(core).

% FORMULA OF SUMS OF POWERS

h_coefficient(C, H):- frac_sum(C, 1/1, R), fraction_comp(R, N, D), fraction_comp(H, D, N).

reminder_(_, [], []):- !.
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
power_sums_(D, SUM, L):- 
	D1 is D - 1,
	power_sums_(D1, S1, L1),
	
	first(S1, S1F, S1R),
	monomial_coefficient(S1F, FH), h_coefficient(FH, H),	% H = 1/(1 + c_{d + 1})

	first(S1R, S1RF, S1RR),
	
	monomial_coefficient(S1RF, COEF),
	polynomial_evaluation((n + 1 - COEF), B),				% B = (n + 1 - c_d)
	polynomial_monomials(B, BMS),

	list_polynomial_prod_list(BMS, S1, S),					% S = B*p(n, d)
	
	first(L1, _, L1R), reminder(L1R, S1RR, R),				% R = sum[j=1->d] c_{d - j}*p(n, d - j)

	list_polynomial_sub_list(S, R, S_MINUS_R),
	list_polynomial_prod_list([H], S_MINUS_R, SUM),			% SUM = H*(S - R)
	
	concat([[D,SUM]], L1, L),
	!.

% S = f(n) = 1^D + 2^D + ... + n^D
power_sums(D, S):- power_sums_(D, SS, _), list_polynomial(SS, S).

% L = [i, f(n, i)] where f(n, i) = 1^i + 2^i + ... + n^i
power_sums_list(D, L):- power_sums_(D, _, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reminder2_(_, [], []):- !.
reminder2_([[D, F]|_], [M], R):-
	monomial_degree(M, D), monomial_coefficient(M, MC),
	list_polynomial_prod_sorted_list([MC], F, R), !.
reminder2_([_|Fs], Ms, R):- reminder2_(Fs, Ms, R), !.
reminder2_([[D, F]|Fs], [M|Ms], R):-
	monomial_degree(M, D), monomial_coefficient(M, MC),
	list_polynomial_prod_sorted_list([MC], F, R1),
	reminder2_(Fs, Ms, R2),
	list_polynomial_sum_sorted_list(R1, R2, R), !.
reminder2_([_|Fs], Ms, R):- reminder2_(Fs, Ms, R).

reminder2(F, P, R):- reminder2_(F, P, R).

power_sums2_(1, [(1/2)*n^2 ,(1/2)*n], [[1, [(1/2)*n^2, (1/2)*n]]]):- !.
power_sums2_(D, SUM, L):- 
	D1 is D - 1,
	power_sums2_(D1, S1, L1),								%write('D='), write(D), nl,
															%write('    D1='), write(D1), nl,
															%write('    L1='), write(L1), nl,
	
	first(S1, S1F, S1R),
	monomial_coefficient(S1F, FH), h_coefficient(FH, H),	% H = 1/(1 + c_{d + 1})

	first(S1R, S1RF, S1RR),
	
	monomial_coefficient(S1RF, COEF),
	polynomial_evaluation((n + 1 - COEF), B),				% B = (n + 1 - c_d)
	polynomial_monomials(B, BMS),							%write('    BMS='), write(BMS), nl,

	list_polynomial_prod_sorted_list(BMS, S1, S),			% S = B*p(n, d)
															%write('    S='), write(S), nl,
	
	first(L1, _, L1R), reminder2(L1R, S1RR, R),				% R = sum[j=1->d] c_{d - j}*p(n, d - j)
															%write('    R='), write(R), nl,

	list_polynomial_sub_sorted_list(S, R, S_MINUS_R),		%write('    S_MINUS_R='), write(S_MINUS_R), nl,
	list_polynomial_prod_sorted_list([H], S_MINUS_R, SUM),	% SUM = H*(S - R)
	
	concat([[D,SUM]], L1, L),
	!.

% S = f(n) = 1^D + 2^D + ... + n^D
power_sums2(D, S):- power_sums2_(D, SS, _), list_polynomial(SS, S).

% L = [i, f(n, i)] where f(n, i) = 1^i + 2^i + ... + n^i
power_sums2_list(D, L):- power_sums2_(D, _, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Computes iteratively the sum: 1^D + 2^D + ... + N^D
sum_from_1_to_n_to_D(1, _, 1):- !.
sum_from_1_to_n_to_D(N, D, S):-
	N1 is N - 1, sum_from_1_to_n_to_D(N1, D, S1),
	S is S1 + N^D.

%D is 3, power_sums(D, S1), power_sums2(D, S2).