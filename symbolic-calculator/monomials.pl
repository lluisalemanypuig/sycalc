:-include(numeric).

% MONOMIALS

% A monomial consists of a variable multiplied by a reduced numerical value (at the left of the variable).
% The variable may have an exponent which is also a numerical value (at the right of the variable).
% The coefficient or the exponent may be inexistent (= 1), but the variable can not.
% A reduced numerical value is the result of eval(E) where E is an arithmetic expression.

monomial_components(- (M), MC, V, E):- monomial_components(M, C, V, E), MC is -C, !.
monomial_components(C*X^E, CE, X, EE):- eval(C, CE), real(CE), eval(E, EE), real(EE), !.
monomial_components(X^E, 1, X, EE):- eval(E, EE), real(EE), !.
monomial_components(C*X, CE, X, 1):- eval(C, CE), real(CE), !.
monomial_components(C, CE, _, 0):- eval(C, CE), real(CE), !.
monomial_components(X, 1, X, 1):- not(expr(X)).

% Monomial definition

monomial(_ + _):- !, false.
monomial(_ - _):- !, false.
monomial(M):- monomial_components(M, C, _, E), eval(C, CE), real(CE), eval(E, EE), real(EE).

monomial_degree(M, D):- monomial_components(M, _, _, D).

% Reduction of monomials

monomial_reduced__(0, _, _, 0):- !.
monomial_reduced__(1, _, 0, 1):- !.
monomial_reduced__(1, V, 1, V):- !.
monomial_reduced__(1, V, E, V^E):- !.
monomial_reduced__(C, _, 0, C):- !.
monomial_reduced__(C, V, 1, C*V):- !.
monomial_reduced__(C, V, E, C*V^E):- !.

monomial_reduced_components(A/B, V, M/N, R):- reduced_fraction(A/B, C), reduced_fraction(M/N, E), monomial_reduced__(C, V, E, R), !.
monomial_reduced_components(A/B, V, E, R):- reduced_fraction(A/B, C), monomial_reduced__(C, V, E, R), !.
monomial_reduced_components(C, V, A/B, R):- reduced_fraction(A/B, E), monomial_reduced__(C, V, E, R), !.
monomial_reduced_components(C, V, E, R):- monomial_reduced__(C, V, E, R).

monomial_reduced(M, R):- monomial_components(M, C, V, E), monomial_reduced_components(C, V, E, R).

%					  C1, V1, E1, C2, V2, E2, R
% used when V1 \= V2
pretty_monomials_prod( 0, _,  _,  _, _,  _, 0).
pretty_monomials_prod( _, _,  _,  0, _,  _, 0).
pretty_monomials_prod( 1, _,  0,  1, _,  0, 1).
pretty_monomials_prod( 1, _,  0,  1, Y,  1, Y).
pretty_monomials_prod( 1, X,  1,  1, _,  0, X).
pretty_monomials_prod( 1, X,  1,  1, Y,  1, X*Y).
pretty_monomials_prod(CX, _,  0, CY, _,  0, C):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, _,  0, CY, Y,  1, C*Y):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X,  1, CY, _,  0, C*X):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X,  1, CY, Y,  1, C*X*Y):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X,  1, CY, Y, EY, C*X*(Y^EY)):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X, EX, CY, Y,  1, C*(X^EX)*Y):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X, EX, CY, Y, EY, C*(X^EX)*(Y^EY)):- eval(CX*CY, C).

% ARITHMETIC OPERATIONS WITH MONOMIALS

% ADDITION

monomial_sum(0, M2, M2):- !.
monomial_sum(M1, 0, M1):- !.
monomial_sum(M1, M2, M3 + M4):- 
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V2, E2),
	V1 \= V2,
	write('Warning (monomial_sum): variables of monomials are not equal: '), nl,
	write('    Monomial 1: '), write(M1), write(', variable: '), write(V1), nl,
	write('    Monomial 2: '), write(M2), write(', variable: '), write(V2), nl,
	monomial_reduced_components(C1, V1, E1, M3), monomial_reduced_components(C2, V2, E2, M4), !.

monomial_sum(M1, M2, M3):-
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
	E1 == E2,
	eval(C1 + C2, S), monomial_reduced_components(S, V1, E1, M3), !.

monomial_sum(M1, M2, R):-
	monomial_components(M1, C1, V, E1), monomial_components(M2, C2, V, E2),
	monomial_reduced_components(C1, V, E1, M3), monomial_reduced_components(C2, V, E2, M4),
	monomial_components(M3, _, V, E3), monomial_components(M4, _, V, E4),
	E3 == E4,
	monomial_sum(M3, M4, R).

monomial_sum(M1, M2, M3 + M4):-
	monomial_components(M1, C1, V, E1), monomial_components(M2, C2, V, E2),
	monomial_reduced_components(C1, V, E1, M3), monomial_reduced_components(C2, V, E2, M4).

% SUBSTRACTION

monomial_sub(0, M2, -M2):- !.
monomial_sub(M1, 0, M1):- !.
monomial_sub(M1, M2, M3 - M4):- 
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V2, E2),
	V1 \= V2,
	write('Warning (monomial_sum): variables of monomials are not equal: '), nl,
	write('    Monomial 1: '), write(M1), write(', variable: '), write(V1), nl,
	write('    Monomial 2: '), write(M2), write(', variable: '), write(V2), nl,
	monomial_reduced_components(C1, V1, E1, M3), monomial_reduced_components(C2, V2, E2, M4), !.

monomial_sub(M1, M2, M3):-
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
	E1 == E2,
	eval(C1 - C2, S), monomial_reduced_components(S, V1, E1, M3), !.

monomial_sub(M1, M2, R):-
	monomial_components(M1, C1, V, E1), monomial_components(M2, C2, V, E2),
	monomial_reduced_components(C1, V, E1, M3), monomial_reduced_components(C2, V, E2, M4),
	monomial_components(M3, _, V, E3), monomial_components(M4, _, V, E4),
	E3 == E4,
	monomial_sub(M3, M4, R).

monomial_sub(M1, M2, M3 - M4):-
	monomial_components(M1, C1, V, E1), monomial_components(M2, C2, V, E2),
	monomial_reduced_components(C1, V, E1, M3), monomial_reduced_components(C2, V, E2, M4).

% PRODUCT

monomial_prod(0, _, 0):- !.
monomial_prod(_, 0, 0):- !.
monomial_prod(M1, M2, M3):-
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V2, E2),
	V1 \= V2,
	write('Warning (monomial_product): variables of monomials are not equal: '), nl,
	write('    Monomial 1: '), write(M1), write(', variable: '), write(V1), nl,
	write('    Monomial 2: '), write(M2), write(', variable: '), write(V2), nl,
	
	monomial_reduced_components(C1, V1, E1, RM1), monomial_reduced_components(C2, V2, E2, RM2),
	monomial_components(RM1, RC1, _, RE1), monomial_components(RM2, RC2, _, RE2),
	pretty_monomials_prod(RC1, V1, RE1, RC2, V2, RE2, M3), !.

monomial_prod(M1, M2, M3):-
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V1, E2),
	eval(C1*C2, C), sum(E1, E2, E), monomial_reduced_components(C, V1, E, M3).


%% DEBUG

red_mon(M):- write(M), write(': '), monomial_reduced(M, R), write(R), nl.
mon_sum(M1, M2):- write(M1), write(' + '), write(M2), write('= '), monomial_sum(M1, M2, S), write(S), nl.
mon_sub(M1, M2):- write(M1), write(' - '), write(M2), write('= '), monomial_sub(M1, M2, S), write(S), nl.
mon_prod(M1, M2):- write(M1), write(' * '), write(M2), write('= '), monomial_prod(M1, M2, S), write(S), nl.

debug_monomials:-
		write('MONOMIAL REDUCTION'), nl, 
		red_mon(0*x^0),
		red_mon(0*x^1),
		red_mon(0*x^5),
		
		red_mon(1*x^0),
		red_mon(1*x^1),
		red_mon(1*x^5),
		
		red_mon(3*x^0),
		red_mon(3*x^1),
		red_mon(3*x^6),
		
		red_mon((0/1)*x^(0/1)),
		red_mon((0/8)*x^(4/4)),
		red_mon((0/3)*x^(20/4)),
		
		red_mon((7/7)*x^(0/9)),
		red_mon((6/6)*x^(2/2)),
		red_mon((1/1)*x^(5/1)),
		
		red_mon((27/9)*x^(0/1)),
		red_mon((18/6)*x^(8/8)),
		red_mon((15/5)*x^(36/6)),

		write('------------'), nl,
		write('MONOMIAL SUM'), nl, 

		mon_sum(3*x, 2*x),
		mon_sum(3*x, 2*x^0),
		mon_sum(3*x, 2*x^1),
		mon_sum(3*x^0, 2*x),
		mon_sum(3*x^1, 2*x),
		mon_sum(3*x^2, 2*x),
		mon_sum(3*x^2, 2*x^2),
		mon_sum(3*x^0, 2*x^0),
		mon_sum(3*x^0, 0*x^2),
		mon_sum(0*x^1, 0*x^2),
		mon_sum(0, 0*x^2),
		mon_sum(0, 0),

		write('------------'), nl,
		write('MONOMIAL SUB'), nl, 

		mon_sub(3*x, 2*x),
		mon_sub(3*x, 2*x^0),
		mon_sub(3*x, 2*x^1),
		mon_sub(3*x^0, 2*x),
		mon_sub(3*x^1, 2*x),
		mon_sub(3*x^2, 2*x),
		mon_sub(3*x^2, 2*x^2),
		mon_sub(3*x^0, 2*x^0),
		mon_sub(3*x^0, 0*x^2),
		mon_sub(0*x^1, 0*x^2),
		mon_sub(0, 0*x^2),
		mon_sub(0, 0),

		write('------------'), nl,
		write('MONOMIAL PROD'), nl, 

		mon_prod(3*x, 2*x),
		mon_prod(3*x, 2*x^0),
		mon_prod(3*x, 2*x^1),
		mon_prod(3*x^0, 2*x),
		mon_prod(3*x^1, 2*x),
		mon_prod(3*x^2, 2*x),
		mon_prod(3*x^2, 2*x^2),
		mon_prod(3*x^0, 2*x^0),
		mon_prod(3*x^0, 0*x^2),
		mon_prod(0*x^1, 0*x^2),
		mon_prod(0, 0*x^2),
		mon_prod(0, 0),
		!.