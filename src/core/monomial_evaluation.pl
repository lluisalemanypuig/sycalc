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

mon_prod(0, _, 0):- !.
mon_prod(_, 0, 0):- !.
mon_prod(M1, M2, PROD):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	red_monomial_comps(C1,V1,E1, RC1,RV1,RE1),
	red_monomial_comps(C2,V2,E2, RC2,RV2,RE2),
	pfuse(RV1,RE1, RV2,RE2, MV,ME),
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

