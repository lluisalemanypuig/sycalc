:-ensure_loaded(arithmetic_evaluation).

% MONOMIALS

% A monomial consists of a variable multiplied by a red numerical value (at the left of the variable).
% The variable may have an exponent which is also a numerical value (at the right of the variable).
% The coefficient or the exponent may be inexistent (= 1), but the variable can not.
% A red numerical value is the result of eval(E) where E is an arithmetic expression.

monomial_comps(- (M), NC, V, E):- monomial_comps(M, C, V, E), rational(C), neg_frac(C, NC), !.
monomial_comps(- (M), NC, V, E):- monomial_comps(M, C, V, E), NC is -C, !.
monomial_comps(C*X^E, CE, X, EE):- arithmetic_eval(C, CE), arithmetic_eval(E, EE), !.
monomial_comps(X^E, 1, X, EE):- arithmetic_eval(E, EE), !.
monomial_comps(C*X, CE, X, 1):- arithmetic_eval(C, CE), !.
monomial_comps(C, CE, _, 0):- arithmetic_eval(C, CE), !.
monomial_comps(X, 1, X, 1):- not(expr(X)).

% Monomial definition

monomial(M):- monomial_comps(M, C, _, E), arithmetic_eval(C, _), arithmetic_eval(E, _).

monomial_neg(M, CN*V^E):- monomial_comps(M, C, V, E), CN is -C.

monomial_degree(M, D):- monomial_comps(M, _, _, D).

% Reduction of monomials

monomial_red__( 0, _, _, 0):- !.
monomial_red__( 1, _, 0, 1):- !.
monomial_red__( 1, V, 1, V):- !.
monomial_red__( 1, V, E, V^E):- !.
monomial_red__(-1, _, 0, -1):- !.
monomial_red__(-1, V, 1, -V):- !.
monomial_red__(-1, V, E, -V^E):- !.
monomial_red__( C, _, 0, C):- !.
monomial_red__( C, V, 1, C*V):- !.
monomial_red__( C, V, E, C*V^E):- !.

monomial_red_comps(C, V, E, R):- arithmetic_eval(C, CE), arithmetic_eval(E, EE), monomial_red__(CE, V, EE, R).

monomial_red(M, R):- monomial_comps(M, C, V, E), monomial_red_comps(C, V, E, R).

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
