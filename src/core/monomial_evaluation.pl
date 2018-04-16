:-ensure_loaded(monomial).

% -----------
% ARITHMETIC OPERATIONS WITH MONOMIALS

%% ADDITION

% addition of two monomials that can or can not be fused into a single one
% when the monomials can not be fused then the result is sorted

% null coeficients
mon_sum_( _,C1,[],[],   _,C2,[],[], C):- arithmetic_eval(C1+C2, C), !.
mon_sum_(M1, _, _, _,  M2,C2,[],[], M1 + M2):- C2 > 0, !.
mon_sum_(M1, _, _, _,   _,C2,[],[], M1 - NC):- C2 < 0, !, rational_neg(C2,NC).
mon_sum_( _,C1,[],[],  M2, _, _, _, M2 + C1):- C1 > 0, !.
mon_sum_( _,C1,[],[],  M2, _, _, _, M2 - NC):- C1 < 0, !, rational_neg(C1,NC).
mon_sum_( _,C1,V1,E1,   _,C2,V1,E1, SUM):-
	red_monomial_from_comps(C1+C2,V1,E1, SUM), !.
mon_sum_(M1,C1, _, _,  M2, _, _, _, M2 + M1):-
	not(monomial_comp(M1,M2)), C1 > 0, !.
mon_sum_(M1,C1, _, _,  M2, _, _, _, M2 - N1):-
	not(monomial_comp(M1,M2)), C1 < 0, monomial_neg(M1, N1), !.
mon_sum_(M1, _, _, _,  M2,C2, _, _, M1 + M2):-
	monomial_comp(M1,M2), C2 > 0, !.
mon_sum_(M1, _, _, _,  M2,C2, _, _, M1 - N2):-
	monomial_comp(M1,M2), C2 < 0, monomial_neg(M2, N2), !.

mon_sum(0, M2, M3):- red_monomial(M2, M3), !.
mon_sum(M1, 0, M3):- red_monomial(M1, M3), !.
mon_sum(M1, M2, R):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	red_monomial_comps(C1,V1,E1, RC1,RV1,RE1),
	red_monomial_comps(C2,V2,E2, RC2,RV2,RE2),

	red_monomial_from_comps(RC1,RV1,RE1, RM1),
	red_monomial_from_comps(RC2,RV2,RE2, RM2),

	mon_sum_(RM1,RC1,RV1,RE1,  RM2,RC2,RV2,RE2,  R).

mon_sum([M1,M2], S):- mon_sum(M1, M2, S).

%% SUBSTRACTION

% substraction of two monomials that can or can not be fused into a single one
% when the monomials can not be fused then the result is sorted

mon_sub(0, M2, M3):- monomial_neg(M2, M3), !.
mon_sub(M1, 0, M3):- red_monomial(M1, M3), !.
mon_sub(M1, M2, R):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	red_monomial_comps(C1,V1,E1, RC1,RV1,RE1),
	red_monomial_comps(C2,V2,E2, RC2,RV2,RE2),
	rational_neg(RC2, N2),
	red_monomial_from_comps(RC1,RV1,RE1, RM1),
	red_monomial_from_comps( N2,RV2,RE2, RM2),
	mon_sum_(RM1,RC1,RV1,RE1, RM2,N2,RV2,RE2, R).

mon_sub([M1,M2], S):- mon_sub(M1, M2, S), !.

%% PRODUCT

/*
% multiplies two monomials assuming their variables are different.
% the result is a multivariate monomial (this is the only predicate to
% produce this kind of monomials - the rest are always univariate).
%						   C1, V1, E1, C2, V2, E2, R
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
	red_monomial_comps(C, V1, E, P), !.
mon_prod_( _,C1,V1,E1,  _,C2,V2,E2, P):-
	red_monomial_comps(C1, V1, E1, RM1),
	red_monomial_comps(C2, V2, E2, RM2),
	monomial_comps(RM1, RC1, _, RE1),
	monomial_comps(RM2, RC2, _, RE2),
	pretty_monomials_prod_comp(RC1, V1, RE1, RC2, V2, RE2, P).
*/

mon_prod(0, _, 0):- !.
mon_prod(_, 0, 0):- !.
mon_prod(M1, M2, PROD):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	red_monomial_comps(C1,V1,E1, RC1,RV1,RE1),
	red_monomial_comps(C2,V2,E2, RC2,RV2,RE2),
	pmerge(RV1,RE1, RV2,RE2, MV,ME),
	collapse_vars_list(MV,ME, CV,CE),
	arithmetic_eval(RC1*RC2, CP),
	red_monomial_from_comps(CP,CV,CE, PROD).

mon_prod([M1,M2], P):- mon_prod(M1, M2, P), !.

% MONOMIAL EVALUATION

% Evaluate the monomial at variable 'V' with value 'VAL'. If variable
% does not exist then the monomial is evaluated to itself
% VAL: real value
% M: reduced monomial
% E: M(VAL)
monomial_evaluation(VAL, V, M, E):-
	monomial_comps(M, C,Vs,Es),
	monomial_exponent(V, Vs,Es, Vr,Er, EXP),
	arithmetic_eval(C*(VAL^EXP), R),
	red_monomial_from_comps(R,Vr,Er, E), !.
monomial_evaluation(  _, _, M, M).

