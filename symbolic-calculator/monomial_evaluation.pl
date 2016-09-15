:-ensure_loaded(arithmetic_evaluation).
:-ensure_loaded(monomials).

%					  C1, V1, E1, C2, V2, E2, R
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO BE USED ONLY WHEN V1 \= V2 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pretty_monomials_prod_comp( 0, _,  _,  _, _,  _, 0).
pretty_monomials_prod_comp( _, _,  _,  0, _,  _, 0).
pretty_monomials_prod_comp( 1, _,  0,  1, _,  0, 1).
pretty_monomials_prod_comp( 1, _,  0,  1, Y,  1, Y).
pretty_monomials_prod_comp( 1, X,  1,  1, _,  0, X).
pretty_monomials_prod_comp( 1, X,  1,  1, Y,  1, X*Y).
pretty_monomials_prod_comp(CX, _,  0, CY, _,  0, C):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX, _,  0, CY, Y,  1, C*Y):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX, X,  1, CY, _,  0, C*X):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX, X,  1, CY, Y,  1, C*X*Y):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX, X,  1, CY, Y, EY, C*X*(Y^EY)):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX, X, EX, CY, Y,  1, C*(X^EX)*Y):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX, X, EX, CY, Y, EY, C*(X^EX)*(Y^EY)):- arithmetic_eval(CX*CY, C).

% -----------
% ARITHMETIC OPERATIONS WITH MONOMIALS

% ADDITION

mon_sum(0, M2, M3):- monomial_red(M2, M3), !.
mon_sum(M1, 0, M3):- monomial_red(M1, M3), !.
mon_sum(M1, -(M2), R):- mon_sub(M1, M2, R), !.
mon_sum(M1, M2, M3 + M4):-
	monomial_comps(M1, C1, V1, E1), monomial_comps(M2, C2, V2, E2),
	V1 \= V2,
	write('Warning (mon_sum): variables of monomials are not equal: '), nl,
	write('    Monomial 1: '), write(M1), write(', variable: '), write(V1), nl,
	write('    Monomial 2: '), write(M2), write(', variable: '), write(V2), nl,
	monomial_red_comps(C1, V1, E1, M3), monomial_red_comps(C2, V2, E2, M4), !.

mon_sum(M1, M2, M3):-
	monomial_comps(M1, C1, V1, E1), monomial_comps(M2, C2, _, E2),
	E1 == E2,
	arithmetic_eval(C1 + C2, S), !, monomial_red_comps(S, V1, E1, M3).

mon_sum(M1, M2, R):-
	monomial_red(M1, M3), monomial_red(M2, M4),
	monomial_comps(M3, _, V, E3), monomial_comps(M4, _, V, E4),
	E3 == E4, !,
	mon_sum(M3, M4, R).

mon_sum(M1, M2, M3 + M4):-
	monomial_comps(M1, C1, V, E1), monomial_comps(M2, C2, V, E2),
	monomial_red_comps(C1, V, E1, M3), monomial_red_comps(C2, V, E2, M4).

mon_sum([M1,M2], S):- mon_sum(M1, M2, S).

% SUBSTRACTION

mon_sub(0, M2, M3):- monomial_comps(M2, C, V, E), K is -C, monomial_red_comps(K, V, E, M3), !.
mon_sub(M1, 0, M3):- monomial_red(M1, M3), !.
mon_sub(M1, M2, M3 - M4):-
	monomial_comps(M1, C1, V1, E1), monomial_comps(M2, C2, V2, E2),
	V1 \= V2,
	write('Warning (mon_sum): variables of monomials are not equal: '), nl,
	write('    Monomial 1: '), write(M1), write(', variable: '), write(V1), nl,
	write('    Monomial 2: '), write(M2), write(', variable: '), write(V2), nl,
	monomial_red_comps(C1, V1, E1, M3), monomial_red_comps(C2, V2, E2, M4), !.

mon_sub(M1, M2, M3):-
	monomial_comps(M1, C1, V1, E1), monomial_comps(M2, C2, _, E2),
	E1 == E2,
	arithmetic_eval(C1 - C2, S), !, monomial_red_comps(S, V1, E1, M3).

mon_sub(M1, M2, R):-
	monomial_red(M1, M3), monomial_red(M2, M4),
	monomial_comps(M3, _, V, E3), monomial_comps(M4, _, V, E4),
	E3 == E4, !,
	mon_sub(M3, M4, R).

mon_sub(M1, M2, M3 - M4):-
	monomial_comps(M1, C1, V, E1), monomial_comps(M2, C2, V, E2),
	monomial_red_comps(C1, V, E1, M3), monomial_red_comps(C2, V, E2, M4).

mon_sub([M1,M2], S):- mon_sub(M1, M2, S).

% PRODUCT

mon_prod(0, _, 0):- !.
mon_prod(_, 0, 0):- !.
mon_prod(M1, M2, M3):-
	monomial_comps(M1, C1, V1, E1), monomial_comps(M2, C2, V2, E2),
	V1 \= V2,
	write('Warning (mon_product): variables of monomials are not equal: '), nl,
	write('    Monomial 1: '), write(M1), write(', variable: '), write(V1), nl,
	write('    Monomial 2: '), write(M2), write(', variable: '), write(V2), nl,
	
	monomial_red_comps(C1, V1, E1, RM1), monomial_red_comps(C2, V2, E2, RM2),
	monomial_comps(RM1, RC1, _, RE1), monomial_comps(RM2, RC2, _, RE2),
	pretty_monomials_prod(RC1, V1, RE1, RC2, V2, RE2, M3), !.

mon_prod(M1, M2, M3):-
	monomial_comps(M1, C1, V1, E1), monomial_comps(M2, C2, V1, E2),
	arithmetic_eval(C1*C2, C), arithmetic_eval(E1 + E2, E), monomial_red_comps(C, V1, E, M3).

mon_prod([M1,M2], S):- mon_prod(M1, M2, S).
