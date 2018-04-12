:-ensure_loaded(monomial).

% -----------
% ARITHMETIC OPERATIONS WITH MONOMIALS

%% ADDITION

% sorted addition of two monomials that cannot be 'fused' into a single
% one:
% if the variables are different, sort by variable
% if the variables are equal but exponents are different, sort by
% exponent
% if the variables and the exponents are equal the result is a single
% monomial -> this case does not apply

% null coeficients
mon_sum_( _, 0, _, _,   _, 0, _, _, 0):- !.
mon_sum_( _, 0, _, _,   _, C, V, E, M):-
	red_univariate_monomial_comps(C,V,E, M), !.
mon_sum_( _, C, V, E,   _, 0, _, _, M):-
	red_univariate_monomial_comps(C,V,E, M), !.
% null exponents
mon_sum_( _,C1, _, 0,   _,C2, _, 0, R):- arithmetic_eval(C1 + C2,R), !.
mon_sum_( M, _, _, _,   _,C2, _, 0, M + C2):- !.
mon_sum_( _,C1, _, 0,   M, _, _, _, C1 + M):- !.
% same variables, same exponents
mon_sum_( _,C1,V1,E1,   _,C2,V1,E1,  R):-
	red_univariate_monomial_comps(C1 + C2, V1, E1, R), !.
% same variables, different exponents
mon_sum_(M1, _,V1,E1,  M2,C2,V1,E2,  M1 + M2):-
	E1 > E2, C2 > 0, !.
mon_sum_(M1, _,V1,E1,   _,C2,V1,E2,  M1 - N2):-
	E1 > E2, C2 < 0,
	rational_neg(C2,K),
	red_univariate_monomial_comps(K,V1,E2, N2), !.
mon_sum_(M1,C1,V1,E1,  M2, _,V1,E2,  M2 + M1):-
	E1 < E2, C1 > 0, !.
mon_sum_( _,C1,V1,E1,  M2, _,V1,E2,  M2 - N1):-
	E1 < E2, C1 < 0,
	rational_neg(C1,K),
	red_univariate_monomial_comps(K,V1,E1, N1), !.
% different variables
mon_sum_(M1, _,V1, _,  M2,C2,V2, _, M1 + M2):- C2 > 0, V1 @< V2, !.
mon_sum_(M1, _,V1, _,  M2,C2,V2, _, M2 + M1):- C2 > 0, V2 @< V1, !.
mon_sum_(M1, _, _, _,   _,C2,V2,E2, M1	- N2):-
	rational_neg(C2, K),
	red_univariate_monomial_comps(K,V2,E2, N2), !.

mon_sum(0, M2, M3):- red_monomial(M2, M3), !.
mon_sum(M1, 0, M3):- red_monomial(M1, M3), !.
mon_sum(M1, M2, R):-
	univariate_monomial_comps(M1, C1, V1, E1),
	univariate_monomial_comps(M2, C2, V2, E2),
	mon_sum_(M1,C1,V1,E1,  M2,C2,V2,E2,  R).

mon_sum([M1,M2], S):- mon_sum(M1, M2, S), !.

%% SUBSTRACTION

% sorted substraction of two monomials that cannot be 'fused' into a
% single one:
% if the variables are different, sort by variable
% if the variables are equal but exponents are different, sort by
% exponent
% if the variables and the exponents are equal the result is a
% single monomial -> this case does not apply

% null coeficients
mon_sub_( _, 0, _, _,   _, 0, _, _, 0):- !.
mon_sub_( _, 0, _, _,   _, C, V, E, M):-
	rational_neg(C,K), red_univariate_monomial_comps(K,V,E, M), !.
mon_sub_( _, C, V, E,   _, 0, _, _, M):-
	red_univariate_monomial_comps(C,V,E, M), !.
% null exponents
mon_sub_( _,C1, _, 0,   _,C2, _, 0, R):- arithmetic_eval(C1 - C2,R), !.
mon_sub_( M, _, _, _,   _,C2, _, 0, M - C2):- !.
mon_sub_( _,C1, _, 0,   M,C2, _, _, C1 - M):- C2 > 0, !.
mon_sub_( _,C1, _, 0,   _,C2,V2,E2, C1 + N):-
	C2 < 0, rational_neg(C2,K), red_univariate_monomial_comps(K,V2,E2, N), !.
% same variables, same exponents
mon_sub_( _,C1,V1,E1,   _,C2,V1,E1,  R):-
	red_univariate_monomial_comps(C1 - C2, V1, E1, R), !.
% same variables, different exponents
mon_sub_(M1, _,V1,E1,  M2,C2,V1,E2,  M1 - M2):-
	E1 > E2, C2 > 0, !.
mon_sub_(M1, _,V1,E1,   _,C2,V1,E2,  M1 - N2):-
	E1 > E2, C2 < 0,
	rational_neg(C2,K), red_univariate_monomial_comps(K,V1,E2, N2), !.
mon_sub_(M1,C1,V1,E1,  M2, _,V1,E2, M2 + M1):-
	E1 < E2, C1 > 0, !.
mon_sub_( _,C1,V1,E1,  M2, _,V1,E2, M2 - N1):-
	E1 < E2, C1 < 0,
	rational_neg(C1,K), red_univariate_monomial_comps(K,V1,E1, N1), !.
% different variables
mon_sub_(M1, _,V1, _,  M2,C2,V2, _, M1 - M2):-
	V1 @< V2, C2 > 0, !.
mon_sub_(M1, _,V1, _,   _,C2,V2,E2, M1 + N1):-
	V1 @< V2, C2 < 0,
	rational_neg(C2,K), red_univariate_monomial_comps(K,V2,E2, N1), !.
mon_sub_(M1,C1,V1, _,   _,C2,V2,E2, N2 + M1):-
	V1 @> V2, C1 > 0,
	rational_neg(C2,K), red_univariate_monomial_comps(K,V2,E2, N2), !.
mon_sub_( _,C1,V1,E1,   _,C2,V2,E2, N2  - N1):-
	V1 @> V2, C1 < 0,
	rational_neg(C1,K1), red_univariate_monomial_comps(K1,V1,E1, N1),
	rational_neg(C2,K2), red_univariate_monomial_comps(K2,V2,E2, N2), !.

mon_sub(0, M2, M3):- monomial_neg(M2, M3), !.
mon_sub(M1, 0, M3):- red_monomial(M1, M3), !.
mon_sub(M1, M2, R):-
	univariate_monomial_comps(M1, C1, V1, E1),
	univariate_monomial_comps(M2, C2, V2, E2),
	mon_sub_(M1,C1,V1,E1,  M2,C2,V2,E2,  R).

mon_sub([M1,M2], S):- mon_sub(M1, M2, S), !.

%% PRODUCT

% multiplies two monomials assuming their variables are different.
% the result is a multivariate monomial (this is the only predicate to
% produce this kind of monomials - the rest are always univariate).
%		   				   C1, V1, E1, C2, V2, E2, R
pretty_monomials_prod_comp( 0,  _,  _,  _,  _,  _, 0).
pretty_monomials_prod_comp( _,  _,  _,  0,  _,  _, 0).
pretty_monomials_prod_comp( 1,  _,  0,  1,  _,  0, 1).
pretty_monomials_prod_comp( 1,  _,  0,  1,  Y,  1, Y).
pretty_monomials_prod_comp( 1,  X,  1,  1,  _,  0, X).
pretty_monomials_prod_comp( 1,  X,  1,  1,  Y,  1, X*Y):- X @< Y, !.
pretty_monomials_prod_comp( 1,  X,  1,  1,  Y,  1, Y*X):- !.
pretty_monomials_prod_comp(CX,  _,  0, CY,  _,  0, C):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  _,  0, CY,  Y,  1, C*Y):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X,  1, CY,  _,  0, C*X):- arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X,  1, CY,  Y,  1, C*X*Y):- X @< Y, arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X,  1, CY,  Y,  1, C*Y*X):- X @> Y, arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X,  1, CY,  Y, EY, C*X*(Y^EY)):- X @< Y, arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X,  1, CY,  Y, EY, C*(Y^EY)*X):- X @> Y, arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X, EX, CY,  Y,  1, C*(X^EX)*Y):- X @< Y, arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X, EX, CY,  Y,  1, C*Y*(X^EX)):- X @> Y, arithmetic_eval(CX*CY, C), !.
pretty_monomials_prod_comp(CX,  X, EX, CY,  Y, EY, C*(X^EX)*(Y^EY)):- X @< Y, arithmetic_eval(CX*CY, C).
pretty_monomials_prod_comp(CX,  X, EX, CY,  Y, EY, C*(Y^EY)*(X^EX)):- arithmetic_eval(CX*CY, C).

mon_prod_( _,C1,V1,E1,  _,C2,V1,E2, P):-
	arithmetic_eval(C1*C2, C), arithmetic_eval(E1 + E2, E),
	red_univariate_monomial_comps(C, V1, E, P), !.
mon_prod_( _,C1,V1,E1,  _,C2,V2,E2, P):-
	red_univariate_monomial_comps(C1, V1, E1, RM1),
	red_univariate_monomial_comps(C2, V2, E2, RM2),
	univariate_monomial_comps(RM1, RC1, _, RE1),
	univariate_monomial_comps(RM2, RC2, _, RE2),
	pretty_monomials_prod_comp(RC1, V1, RE1, RC2, V2, RE2, P).

mon_prod(0, _, 0):- !.
mon_prod(_, 0, 0):- !.
mon_prod(M1, M2, P):-
	univariate_monomial_comps(M1, C1, V1, E1),
	univariate_monomial_comps(M2, C2, V2, E2),
	mon_prod_(M1,C1,V1,E1, M2,C2,V2,E2, P).

mon_prod([M1,M2], S):- mon_prod(M1, M2, S), !.

% MONOMIAL EVALUATION

% Evaluate the monomial if its variable is equal to 'V'.
% Takes a reduced monomial and evaluates it with the value VAL
% VAL: real value
% M: reduced monomial
% E: M(VAL)
monomial_evaluation(VAL, V, M, R):-
	univariate_monomial_comps(M, C, V, EXP),
	arithmetic_eval(C*(VAL^EXP), R).
monomial_evaluation(_, _, M, M).
