:-ensure_loaded(arithmetic_evaluation).
:-ensure_loaded(numbers).

% MONOMIALS

% A monomial consists of a variable multiplied by a red numerical value (at the left of the variable).
% The variable may have an exponent which is also a numerical value (at the right of the variable).
% The coefficient or the exponent may be inexistent (= 1), but the variable can not.
% A red numerical value is the result of eval(E) where E is an arithmetic expression.

monomial_comps(_ + _, _, _, _):- !, false.
monomial_comps(_ - _, _, _, _):- !, false.
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

monomial_comp(M1, M2):- monomial_comps(M1, C1, _, E1), monomial_comps(M2, C2, _, E2), E1 == E2, C1 < C2, !.
monomial_comp(M1, M2):- monomial_comps(M1, _, _, E1), monomial_comps(M2, _, _, E2), E1 < E2.

monomial_sort(L, R):- isort_by(monomial_comp, L, R).

