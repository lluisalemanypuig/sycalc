:-ensure_loaded("../core").
:-ensure_loaded("power_sums").

% MONOMIAL SYMBOLIC EVALUATION

% The symbolic evaluation of monomial M at variable V with expression E
% is the resulting symbolic expression of replacing variable V inside
% monomial M with E.
monomial_symb_evaluation(V,E, M, N):-
	monomial_split(V, M, Vo,Wo),
	monomial_var_exp(Vo, V, V^P),
	polynomial_expression_evaluation( Wo*(E^P), N ).

% MONOMIAL SUMMATION EVALUATION

% The summation of a monomial M over variable v_k from I to F (1) is
% the product of the coefficient and the variables different from v_k of
% the monomial and the difference between the summation of the monomial
% v_i^p_i over variable v_k from 0 to F and the summation of the
% monomial v_i^p_i over variable v_k from 0 to I - 1 (2).
% (1) for I <= F
%     sum_{v_k=I}^{F} M
%     where M = coefficient * {v_i^p_i},
%         v_i is a variable
%         p_i is a rational value
% (1.1)
%     if v_k is the k-th variable then M can be denoted as
%         M = c*v_1^p_1*...*v_k^p_k*...* v_n^p_n
% (1.2)
%     M without v_k is
%  M(-v_k) = c*v_1^p_1*...*v_{k-1}^p_{k-1}*v_{k+1}^p_{k+1}*...*v_n^p_n
% (1.3)
%     If a monomial does not have variable v_k then
%     M(-v_k) = M
% (2)
%     sum_{v_k=I}^{F} M =
%     M(-v_k)*(sum_{v_k=0}^{F} (v_k^p_k) - sum_{v_k=0}^{I-1} (v_k^p_k))
monomial_summation_(VAR, I,F, M, S):-	% I < F
	monomial_split(VAR, M, Vo,Wo),
	monomial_var_exp(Vo, VAR, VAR^P),

	% SP: power sums for power P
	power_sums(P, PS),

	polynomial_symb_eval(n,F, PS, EPlus),

	polynomial_expression_evaluation(I-1, I1),
	polynomial_symb_eval(n,I1, PS, EMinus),

	polynomial_expression_evaluation(Wo*(EPlus - EMinus), S).

monomial_summation(VAR, I,F, M, S):-
	atom(I), !,
	monomial_summation_(VAR, I,F, M, S).
monomial_summation(VAR, I,F, M, S):-
	atom(F), !,
	monomial_summation_(VAR, I,F, M, S).
monomial_summation(  _, I,F, _, 0):- real(I),real(F), I > F, !.
monomial_summation(VAR, I,F, M, S):-
	real(I),real(F), I =< F,
	monomial_summation_(VAR, I,F, M, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% POLYNOMIAL EXPRESSIONS' EVALUATION %%%%

% Polynomial symbolic evaluation

polynomial_symb_eval(V,E, P, R):-
	list_from_polynomial(P, MS),
	map(monomial_symb_evaluation(V,E), MS, MSR),
	foldl(polynomial_expression_sum, 0, MSR, Q),
	polynomial_expression_evaluation(Q, R).

polynomial_expression_evaluation_list(P, [R]):-
	monomial(P), red_monomial(P, R), !.

polynomial_expression_evaluation_list(Q1 + Q2, R):-
	polynomial_expression_evaluation_list(Q1, L1),
	polynomial_expression_evaluation_list(Q2, L2),
	list_concat(L1, L2, L),
	red_list_monomials(L, R), !.

polynomial_expression_evaluation_list(Q1 - Q2, R):-
	polynomial_expression_evaluation_list(Q1, L1),
	polynomial_expression_evaluation_list(Q2, L2),
	map(monomial_neg, L2, NL2),
	list_concat(L1, NL2, L),
	red_list_monomials(L, R), !.

polynomial_expression_evaluation_list(Q1*Q2, R):-
	polynomial_expression_evaluation_list(Q1, L1),
	polynomial_expression_evaluation_list(Q2, L2),
	cartesian_product(L1, L2, L),
	map(mon_prod, L, PROD),
	red_list_monomials(PROD, R), !.

polynomial_expression_evaluation_list((-Q1)^N, R):-
	polynomial_expression_evaluation_list(Q1, L1),
	map(monomial_neg, L1, NL1),
	polynomial_from_list_power_list(NL1, N, R), !.

polynomial_expression_evaluation_list(Q1^N, R):-
	polynomial_expression_evaluation_list(Q1, L1),
	polynomial_from_list_power_list(L1, N, R), !.

polynomial_expression_evaluation_list(-Q1, R):-
	polynomial_expression_evaluation_list(Q1, L1),
	map(monomial_neg, L1, NL1),
	red_list_monomials(NL1, R), !.

% Convert a binomial into a polynomial.
% I is either a natural number or an arithmetic expression that
% evaluates to a natural number.
polynomial_expression_evaluation_list( choose(P, I), R):-
	factorial(I, F),
	falling_factorial(P, I, FF),
	polynomial_expression_evaluation_list( (1/F)*FF, R ), !.

% Takes a sequence of sums and substractions P of polynomials, contracted
% or expanded, and operates it.
polynomial_expression_evaluation(P, R):-
	polynomial_expression_evaluation_list(P, L), polynomial_from_list(L, R).

polynomial_expression_sum(P, Q, R):-
	polynomial_expression_evaluation_list(P+Q, L), polynomial_from_list(L, R).
polynomial_expression_sub(P, Q, R):-
	polynomial_expression_evaluation_list(P-Q, L), polynomial_from_list(L, R).
polynomial_expression_prod(P, Q, R):-
	polynomial_expression_evaluation_list(P*Q, L), polynomial_from_list(L, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% SYMBOLIC DOT PRODUCT %%%%

% Takes two lists of polynomials and computes the dot product of the two
% lists.
symbolic_dot_prod([X], [Y], P):- polynomial_evaluation(X*Y, P), !.
symbolic_dot_prod([X|Xs], [Y|Ys], R):-
	polynomial_evaluation(X*Y, P),
	symbolic_dot_prod(Xs, Ys, Q),
	polynomial_evaluation(P + Q, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% OTHER POLYNOMIAL USEFUL OPERATIONS %%%%%%%%%

% Constructs the falling factorial polynomial on polynomial P:
% (P - I)*(P - (I - 1))*(P - (I - 2))* .... *(P - 1)*P
falling_factorial(P, 1, FF):- polynomial_expression_evaluation(P, FF), !.
falling_factorial(P, I, FF):-
	I1 is I - 1,
	polynomial_expression_evaluation(P - 1, Pminus1),
	falling_factorial(Pminus1, I1, FF1),
	polynomial_expression_evaluation(P*FF1, FF).

% Takes two polynomials, expanded or contracted, evaluates them, and
% fails if they are not equal
polynomial_eval_eq(P1, P2):-
	polynomial_expression_evaluation(P1, EP1),
	polynomial_expression_evaluation(P2, EP2),
	polynomial_eq(EP1, EP2).

% Takes an expanded polynomial and evaluates it with the value VAL
% on variable X. All those monomials with that variable will be
% evaluated on such variable.
% VAL: real value
% P(x): expanded polynomial
% E: P(VAL)
expanded_polynomial_evaluation(VAL, V, P, E):-
	list_from_polynomial(P, MS),
	map(monomial_value_evaluation(VAL, V), MS, R),
	red_list_monomials(R, L),
	polynomial_from_list(L, E).

% Takes a polynomial and evaluates it with the value VAL
% on variable V. All those monomials with that variable will be
% evaluated on such variable.
% VAL: real value
% P(x): expanded polynomial
% E: P(VAL)
polynomial_value_evaluation(VAL, V, P, E):-
	polynomial_value_evaluation(P, EXP),
	expanded_polynomial_evaluation(VAL, V, EXP, E).

