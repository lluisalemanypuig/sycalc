:-ensure_loaded(monomial).

/***
	@descr This file contains the predicates that define three basic
	arithmetic operations between monomials: addition, substraction
	and multiplication. Also, the evaluation of a monomial, that is,
	the replacement of a variable by a rational value.
*/

%%%
%%% ARITHMETIC OPERATIONS WITH MONOMIALS
%%%

% --------
% ADDITION

% addition of two monomials that can or can not be fused into a single one
% when the monomials can not be fused then the result is sorted

% null coeficients
mon_sum_( _,C1,[],[],   _,C2,[],[], C):- arith_expr_eval(C1+C2, C), !.
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

/**
	@form mon_sum(Mon1, Mon2, Sum)
	@descr @Sum is the sum of @Mon1 and @Mon2.
	\blist
	\item If @Mon1 and @Mon2 do not have the same variables (or have
	the same variables but raised to different exponents) then
	@Sum is an expression that is the arithmetic sum of @Mon1
	and @Mon2. In the expression, the monomial to the left and the
	monomial to the right are so that they make the comparison '@<' true.
	\item If @Mon1 and @Mon2 have the same variables each raised to the
	same exponents then @Sum contains these variables with a leading
	coefficient equal to the sum of @Mon1's and @Mon2's leading coefficient.
	++>
*/
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

/**
	@form mon_sum(MonomialList, Sum)
	@descr @Sum equals the sum of the two elements in @MonomialList.
	Uses predicate ?mon_sum/3.
	@constrs
		@param MonomialList Contains two elements.
*/
mon_sum([M1,M2], S):- mon_sum(M1, M2, S).

% ------------
% SUBSTRACTION

% substraction of two monomials that can or can not be fused into a single one
% when the monomials can not be fused then the result is sorted

/**
	@form mon_sub(Mon1, Mon2, Sub)
	@descr @Sub is the sum of @Mon1 and @Mon2.
	\blist
	\item If @Mon1 and @Mon2 do not have the same variables (or have
	the same variables but raised to different exponents) then
	@Sub is an expression that is the arithmetic substraction of @Mon2
	from @Mon1. In the expression, the monomial to the left and the
	monomial to the right are so that they make the comparison '@<' true.
	\item If @Mon1 and @Mon2 have the same variables each raised to the
	same exponents then @Sub contains these variables with a leading
	coefficient equal to the substraction of @Mon2's from @Mon1's leading
	coefficient.
	++>
*/
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

/**
	@form mon_sub(MonomialList, Sub)
	@descr @Sub equals the sum of the two elements in @MonomialList.
	Uses predicate ?mon_sub/3.
	@constrs
		@param MonomialList Contains two elements.
*/
mon_sub([M1,M2], S):- mon_sub(M1, M2, S), !.

% -------
% PRODUCT

/**
	@form mon_prod(@Mon1, @Mon2, Prod)
	@descr @Prod is the product of @Mon1 and @Mon2.
*/
mon_prod(0, _, 0):- !.
mon_prod(_, 0, 0):- !.
mon_prod(M1, M2, PROD):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	red_monomial_comps(C1,V1,E1, RC1,RV1,RE1),
	red_monomial_comps(C2,V2,E2, RC2,RV2,RE2),
	pfuse(RV1,RE1, RV2,RE2, MV,ME),
	collapse_vars_list(MV,ME, CV,CE),
	arith_expr_eval(RC1*RC2, CP),
	red_monomial_from_comps(CP,CV,CE, PROD).

/**
	@form mon_prod(MonomialList, Prod)
	@descr @Prod is the product of the two elements in @MonomialList.
	Uses predicate ?mon_prod/3.
	@constrs
		@param MonomialList Contains two elements.
*/
mon_prod([M1,M2], P):- mon_prod(M1, M2, P), !.

%%%
%%% MONOMIAL EVALUATION
%%%

% Evaluate the monomial at variable 'V' with value 'VAL'. If variable
% does not exist then the monomial is evaluated to itself
% VAL: real value
% M: reduced monomial
% E: M(VAL)
/**
	@form monomial_value_evaluation(Value,Variable, Monomial, Result)
	@descr @Result is the replacement of @Variable by @Value in @Monomial.
	The replacement of variable 'v_i' by 'q' in monomial
	\bverbatim
		m(V) = c*(v_1^e_1)* ... *(v_i^e_i)* ... *(v_n^e_n)
	\everbatim
	gives
	\bverbatim
		m(V\{v_i}) =
			c*(q^e_i)*
			(v_1^e_1)* ... *(v_{i-1}^e_{i-1})*(v_{i+1}^e_{i+1})* ... *(v_n^e_n)
	\everbatim
	For example, the replacement of 'x' by '3' in '3*x^3*y^2' gives '81*y^2'.
*/
monomial_value_evaluation(VAL, V, M, E):-
	monomial_comps(M, C,Vs,Es),
	monomial_var_exp_comps(V, Vs,Es, Vr,Er, EXP),
	arith_expr_eval(C*(VAL^EXP), R),
	red_monomial_from_comps(R,Vr,Er, E), !.
monomial_value_evaluation(  _, _, M, M).

