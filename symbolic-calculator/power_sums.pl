:-ensure_loaded(core).

h_coefficient(C, H):- arithmetic_eval(C + 1, R), fraction_comp(R, N, D), fraction_comp(H, D, N).

%%%%%%%%%%%%%%%%%%
% power_sums 1

reminder_([M], R):- monomial_degree(M, MD), power_sums(MD, S), monomial_coefficient(M, MC), polynomial_evaluation(MC*S, R), !.
reminder_([M|MS], R):- reminder_([M], R1), reminder_(MS, R2), polynomial_evaluation(R1 + R2, R), !.
reminder(P, R):- polynomial_monomials(P, MS), reminder_(MS, R).

power_sums(1, (1/2)*n^2 + (1/2)*n):- !.
power_sums(D, S):- D1 is D - 1, power_sums(D1, S1),			% sum of (D - 1)-powers
	polynomial_evaluation((n + 1)*S1, SKY),					% write('SKY= '), write(SKY), nl,
	polynomial_first_monomial(S1, F, R), 					% write('    First monomial= '), write(F), nl,
															% write('    Other monomials= '), write(R), nl,
	monomial_coefficient(F, HH), 							% write('    Monomial coefficient= '), write(HH), nl,
	h_coefficient(HH, HUMANS), 								% write('    Humans= '), write(HUMANS), nl,
	reminder(R, REMINDER),									% write('    Reminder= '), write(REMINDER), nl,

	polynomial_evaluation( HUMANS*(SKY - REMINDER), S ),
	!.

%%%%%%%%%%%%%%%%%%
% power_sums 2

reminder2_(_, [], 0):- !.
reminder2_([[D, F]|_], [M], R):-
	%write('reminder2 (1): '), nl,
	%write('    [F]='), write([[D, F]|Fs]), write(' [M]='), write([M]), nl,
	%write('    F='), write([D, F]), write(' M='), write(M), nl,
	monomial_degree(M, D),
	monomial_coefficient(M, MC), polynomial_evaluation(MC*F, R),
	%write('    correct here'), nl,
	!.
reminder2_([_|Fs], Ms, R):-
	%write('reminder2 (3): '), nl,
	%write('    F='), write(Fs), write(' M='), write(Ms), nl,
	reminder2_(Fs, Ms, R),
	!.
reminder2_([[D, F]|Fs], [M|Ms], R):-
	%write('reminder2 (2): '), nl,
	%write('    [F]='), write([[D, F]|Fs]), write(' [M]='), write([M|Ms]), nl,
	%write('    F='), write([D, F]), write(' M='), write(M), nl,
	monomial_degree(M, D),
	monomial_coefficient(M, MC), polynomial_evaluation(MC*F, R1),
	reminder2_(Fs, Ms, R2), polynomial_evaluation(R1 + R2, R),
	%write('    correct here'), nl,
	!.
reminder2_([_|Fs], Ms, R):-
	%write('reminder2 (3): '), nl,
	%write('    F='), write(Fs), write(' M='), write(Ms), nl,
	reminder2_(Fs, Ms, R).

reminder2(F, P, R):- polynomial_monomials(P, MS), reminder2_(F, MS, R).

optimized_power_sums_(1, (1/2)*n^2 + (1/2)*n, [[1, (1/2)*n^2 + (1/2)*n]]):- !.
optimized_power_sums_(D, S, L):- 
	D1 is D - 1, optimized_power_sums_(D1, S1, L1),			% sum of (D - 1)-powers
															%write('D='), write(D1), nl,
	polynomial_evaluation((n + 1)*S1, SKY),					%write('    sky= '), write(SKY), nl,
															%write('    L1='), write(L1), nl,
	polynomial_first_monomial(S1, F, R), 					%write('    First monomial= '), write(F), nl,
															%write('    Pre reminder= '), write(R), nl,
	monomial_coefficient(F, HH), 							%write('    Monomial coefficient= '), write(HH), nl,
	h_coefficient(HH, HUMANS), 								%write('    Humans= '), write(HUMANS), nl,
	reminder2(L1, R, REMINDER),								%write('    Reminder= '), write(REMINDER), nl,

	polynomial_evaluation(HUMANS*(SKY - REMINDER), S),		%write('        Formula='), write(S), nl,
	concat([[D,S]], L1, L),
	!.

optimized_power_sums(D, S):- optimized_power_sums_(D, S, _).


% power_sums(1, S1), power_sums(2, S2), power_sums(3, S3), power_sums(4, S4),
% polynomial_evaluation((5/6)*((n + 1)*S4 - 1/2*S4 - 1/3*S3 + 1/30*S1), S5),
% power_sums(5, S5),