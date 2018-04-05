:-ensure_loaded(number).

% MONOMIALS

% A monomial consists of a variable multiplied by a reduced numerical
% value (at the left of the variable). The variable may have an exponent
% which is also a numerical value (at the right of the variable).
% The coefficient or the exponent may be inexistent (= 1), but the variable
% can not. A reduced numerical value is the result of arithmetic_eval(E)
% where E is an arithmetic expression.

% Extracts the components of a monomial in reduced form:
% (2 + 2)*x^(3 - 1) gives: C = 4, V = x, E = 2
monomial_comps(_ + _, _, _, _):- !, false.
monomial_comps(_ - _, _, _, _):- !, false.
monomial_comps(- (M), NC, V, E):-
	monomial_comps(M, C, V, E), rational(C), rational_neg(C, NC), !.
monomial_comps(- (M), NC, V, E):- monomial_comps(M, C, V, E), NC is -C, !.
monomial_comps(C*X^E, CE, X, EE):-
	arithmetic_eval(C, CE), arithmetic_eval(E, EE), !.
monomial_comps(X^E, 1, X, EE):- arithmetic_eval(E, EE), !.
monomial_comps(C*X, CE, X, 1):- arithmetic_eval(C, CE), !.
monomial_comps(C, CE, _, 0):- arithmetic_eval(C, CE), !.
monomial_comps(X, 1, X, 1):- not(expr(X)).

% Monomial definition
% If the variable is present, check it is an atom
monomial(M):-
	monomial_comps(M, C, V, E),
	arithmetic_eval(C, _),
	arithmetic_eval(E, _),
	nonvar(V), atom(V), !.
% If the variable is present, but it is not atom -> reject
monomial(M):-
	monomial_comps(M, _, V, _),
	nonvar(V), !, false.
% If the variable is not present, ignore it
monomial(M):-
	monomial_comps(M, C, _, E),
	arithmetic_eval(C, _),
	arithmetic_eval(E, _).

% N = -M, where M is a monomial
monomial_neg(M, N):-
	monomial_comps(M, C, V, E), rational_neg(C, CN),
	red_monomial_comps(CN, V, E, N).

% C (or D) is the coefficient (or the degree) of the monomial M
monomial_coefficient(M, C):- monomial_comps(M, C, _, _).
monomial_degree(M, D):- monomial_comps(M, _, _, D).

% Auxiliar predicate for monomial reduction predicates
red_monomial__( 0, _, _, 0):- !.
red_monomial__( 1, _, 0, 1):- !.
red_monomial__( 1, V, 1, V):- !.
red_monomial__( 1, V, E, V^E):- !.
red_monomial__(-1, _, 0, -1):- !.
red_monomial__(-1, V, 1, -V):- !.
red_monomial__(-1, V, E, -V^E):- !.
red_monomial__( C, _, 0, C):- !.
red_monomial__( C, V, 1, C*V):- !.
red_monomial__( C, V, E, C*V^E):- !.

% R is the reduction of the monomial C*V^E
red_monomial_comps(C, V, E, R):-
	arithmetic_eval(C, CE), arithmetic_eval(E, EE),
	red_monomial__(CE, V, EE, R).

% R is the reduction of the monomial M
red_monomial(M, R):-
	monomial_comps(M, C, V, E), red_monomial_comps(C, V, E, R).

% Monomial comparison (1/2).
% - If the variables are different : M1 < M2 iff V1 < V2
% - If the exponents are equal : M1 < M2 iff C1 < C2 - where Ci is the
%	coefficient of monomial Mi
% - If the exponents are different : M1 < M2 iff E1 > E2 - where Ci is the
%	exponent of monomial Mi
monomial_comp(M1, M2):-
	monomial_comps(M1, _, V1, _), monomial_comps(M2, _, V2, _),
	V1 \= V2, !, V1 @< V2.
monomial_comp(M1, M2):-
	monomial_comps(M1, C1, _, E1), monomial_comps(M2, C2, _, E2),
	E1 is E2, C1 < C2, !.
monomial_comp(M1, M2):-
	monomial_comps(M1, _, _, E1), monomial_comps(M2, _, _, E2), E1 > E2.

% Monomial comparison (2/2).
% - If the variables are different : M1 < M2 iff V1 > V2
% - If the exponents are equal : M1 < M2 iff C1 > C2 - where Ci is the
%	coefficient of monomial Mi
% - If the exponents are different : M1 < M2 iff E1 < E2 - where Ci is
%	the exponent of monomial Mi
monomial_inv_comp(M1, M2):- monomial_comp(M2, M1).

% Sort a list of monomials L into R using different comparisons
monomial_sort(L, R):- isort_by(monomial_comp, L, R).
monomial_inv_sort(L, R):- isort_by(monomial_inv_comp, L, R).

% Does this monomial has a positive coefficient?
monomial_positive_coefficient(M):- monomial_comps(M, C, _, _), C >= 0.

% Replace the variable of the monomial with variable 'X'
monomial_revar(X, M, R):-
	monomial_comps(M, C,_,E), red_monomial(C*X^E, R).
