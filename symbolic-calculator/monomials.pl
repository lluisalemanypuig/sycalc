:-include(numeric).

% MONOMIALS

% A monomial consists of a reduced numerical value multiplied by a
% variable, which may have, in turn, an exponent which is also a
% reduced numerical value. The coefficient or the exponent may be
% inexistent, but the variable can not.

% The reduced numerical value may be represented by a number or 
% a fraction, which does not need to be irreducible

monomial_components(C*X^E, C, X, E):- rational(C), rational(E), !.
monomial_components(X^E*C, C, X, E):- rational(C), rational(E), !.
monomial_components((X*C)^E, C, X, E):- rational(C), rational(E), !.
monomial_components(X^E, 1, X, E):- rational(E), !.
monomial_components(C*X, C, X, 1):- rational(C), !.
monomial_components(X*C, C, X, 1):- rational(C), !.
monomial_components(M, 1, 1, 1):- write('Error (monomial_components): failed when parsing the monomial\'s components. Monomial received: '), write(M), nl, false.

% Reduction of monomials

reduced_monomial__(0, _, _, 0):- !.
reduced_monomial__(1, _, 0, 1):- !.
reduced_monomial__(1, V, 1, V):- !.
reduced_monomial__(1, V, E, V^E):- !.
reduced_monomial__(C, _, 0, C):- !.
reduced_monomial__(C, V, 1, C*V):- !.
reduced_monomial__(C, V, E, C*V^E).

reduced_monomial_(A/B, V, M/N, R):- reduced_fraction(A/B, C), reduced_fraction(M/N, E), reduced_monomial__(C, V, E, R), !.
reduced_monomial_(A/B, V, E, R):- reduced_fraction(A/B, C), reduced_monomial__(C, V, E, R), !.
reduced_monomial_(C, V, A/B, R):- reduced_fraction(A/B, E), reduced_monomial__(C, V, E, R), !.
reduced_monomial_(C, V, E, R):- reduced_monomial__(C, V, E, R).

reduced_monomial(M, R):- monomial_components(M, C, V, E), reduced_monomial_(C, V, E, R).


% Addition of monomials

monomial_sum(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V2, E2),
							V1 \= V2, M3 = C1*V1^E1 + C2*V2^E2, !.

monomial_sum(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
							E1 == E2, sum(C1, C2, S), M3 = S*V1^E1, !.

monomial_sum(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
							E1 > E2, sub(E1, E2, E3), M3 = (C1*V1^E3 + C2*V1)*V1^E2, !.

monomial_sum(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
							sub(E2, E1, E3), M3 = (C1 + C2*V1^E3)*V1^E1.

% Substraction of monomials

monomial_sub(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, V2, E2),
							V1 \= V2, M3 = C1*V1^E1 - C2*V2^E2, !.

monomial_sub(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
							E1 == E2, sub(C1, C2, S), M3 = S*V1^E1, !.

monomial_sub(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
							E1 > E2, sub(E1, E2, E3), M3 = (C1*V1^E3 - C2*V1)*V1^E2, !.

monomial_sub(M1, M2, M3):- 	monomial_components(M1, C1, V1, E1), monomial_components(M2, C2, _, E2),
							sub(E2, E1, E3), M3 = (C1 - C2*V1^E3)*V1^E1.

% Product of monomials

monomial_product(A, B*X, C*X):- C is A*B, !.
monomial_product(A*X, B, C*X):- C is A*B, !.
monomial_product(A*X, B*X, C*X^2):- C is A*B, !.
monomial_product(A*X, B*Y, C*X*Y):- C is A*B, !.
