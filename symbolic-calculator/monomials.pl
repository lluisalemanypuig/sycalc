:-include(numeric).

% MONOMIALS

% A monomial consists of a variable multiplied by a reduced numerical value (at the left of the variable).
% The variable may have an exponent which is also a numerical value (at the right of the variable).
% The coefficient or the exponent may be inexistent (= 1), but the variable can not.
% A reduced numerical value is the result of eval(E) where E is an arithmetic expression.

monomial_components(C*X^E, C, X, E):- !.
monomial_components(X^E, 1, X, E):- !.
monomial_components(C*X, C, X, 1):- !.
monomial_components(C, C, _, 0):- number(C), !.
monomial_components(X, 1, X, 1):- !.

% Monomial definition

monomial(M):- monomial_components(M, _, _, _).

% Reduction of monomials

monomial_reduced__(0, _, _, 0):- !.
monomial_reduced__(1, _, 0, 1):- !.
monomial_reduced__(1, V, 1, V):- !.
monomial_reduced__(1, V, E, V^E):- !.
monomial_reduced__(C, _, 0, C):- !.
monomial_reduced__(C, V, 1, C*V):- !.
monomial_reduced__(C, V, E, C*V^E).

monomial_reduced_components(A/B, V, M/N, R):- reduced_fraction(A/B, C), reduced_fraction(M/N, E), monomial_reduced__(C, V, E, R), !.
monomial_reduced_components(A/B, V, E, R):- reduced_fraction(A/B, C), monomial_reduced__(C, V, E, R), !.
monomial_reduced_components(C, V, A/B, R):- reduced_fraction(A/B, E), monomial_reduced__(C, V, E, R), !.
monomial_reduced_components(C, V, E, R):- monomial_reduced__(C, V, E, R).

monomial_reduced(M, R):- monomial_components(M, C, V, E), monomial_reduced_components(C, V, E, R).

%					  C, V, E, C, V, E, R
pretty_monomials_prod( 0, _,  _,  _, _,  _, 0):- !.
pretty_monomials_prod( _, _,  _,  0, _,  _, 0):- !.
pretty_monomials_prod( 1, _,  0,  1, _,  0, 1):- !.
pretty_monomials_prod( 1, _,  0,  1, Y,  1, Y):- !.
pretty_monomials_prod( 1, X,  1,  1, _,  0, X):- !.
pretty_monomials_prod( 1, X,  1,  1, Y,  1, X*Y):- !.
pretty_monomials_prod(CX, _,  0, CY, _,  0, C):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, _,  0, CY, Y,  1, C*Y):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X,  1, CY, _,  0, C*X):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X,  1, CY, Y,  1, C*X*Y):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X,  1, CY, Y, EY, C*X*(Y^EY)):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X, EX, CY, Y,  1, C*(X^EX)*Y):- eval(CX*CY, C), !.
pretty_monomials_prod(CX, X, EX, CY, Y, EY, C*(X^EX)*(Y^EY)):- eval(CX*CY, C).


% Addition of monomials

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

monomial_sum(M1, M2, M3 + M4):-
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V2, E2),
	monomial_reduced_components(C1, V1, E1, M3), monomial_reduced_components(C2, V2, E2, M4).

% Substraction of monomials

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

monomial_sub(M1, M2, M3 - M4):-
	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V2, E2),
	monomial_reduced_components(C1, V1, E1, M3), monomial_reduced_components(C2, V2, E2, M4).

% Product of monomials

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

