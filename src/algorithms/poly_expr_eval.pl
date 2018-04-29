:-ensure_loaded("../core").

% MONOMIAL SYMBOLIC EVALUATION

% The symbolic evaluation of monomial M at variable V with expression E
% is the resulting symbolic expression of replacing variable V inside
% monomial M with E.
monomial_symb_evaluation(V, E, M, N):-
	monomial_split(V, M, Vo,Wo),
	monomial_var_exp(Vo, V, V^P),
	polynomial_evaluation( Wo*(E^P), N ).

% MONOMIAL SUMMATION EVALUATION

% The summation of a monomial M over variable v_k from I to F (1) is
% the product of the coefficient and the variables different from v_k of
% the monomial and the difference between the summation of the monomial
% v_i^p_i over variable v_k from 0 to F and the summation of the
% monomial v_i^p_i over variable v_k from 0 to I - 1 (2).
% (1)
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
monomial_summation(VAR, I,F, M, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% POLYNOMIAL EXPRESSIONS' EVALUATION %%%%

polynomial_evaluation_list(P, [R]):-
	monomial(P), red_monomial(P, R), !.

polynomial_evaluation_list(Q1 + Q2, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_evaluation_list(Q2, L2),
	list_concat(L1, L2, L),
	list_red_polynomial_from_list(L, R), !.

polynomial_evaluation_list(Q1 - Q2, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_evaluation_list(Q2, L2),
	map(monomial_neg, L2, NL2),
	list_concat(L1, NL2, L),
	list_red_polynomial_from_list(L, R), !.

polynomial_evaluation_list(Q1*Q2, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_evaluation_list(Q2, L2),
	cartesian_product(L1, L2, L),
	map(mon_prod, L, PROD),
	list_red_polynomial_from_list(PROD, R), !.

polynomial_evaluation_list((-Q1)^N, R):-
	polynomial_evaluation_list(Q1, L1),
	map(monomial_neg, L1, NL1),
	polynomial_from_list_power_list(NL1, N, R), !.

polynomial_evaluation_list(Q1^N, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_from_list_power_list(L1, N, R), !.

polynomial_evaluation_list(-Q1, R):-
	polynomial_evaluation_list(Q1, L1),
	map(monomial_neg, L1, NL1),
	list_red_polynomial_from_list(NL1, R), !.

% Convert a binomial into a polynomial.
% I is either a natural number or an arithmetic expression that
% evaluates to a natural number.
polynomial_evaluation_list( choose(P, I), R):-
	factorial(I, F),
	falling_factorial(P, I, FF),
	polynomial_evaluation_list( (1/F)*FF, R ), !.

% Takes a sequence of sums and substractions P of polynomials, contracted
% or expanded, and operates it.
polynomial_evaluation(P, R):-
	polynomial_evaluation_list(P, L), polynomial_from_list(L, R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% SYMBOLIC DOT PRODUCT %%%%

% Takes two lists of polynomials and computes the dot product of the two
% lists.
symbolic_dot_prod([X], [Y], P):- polynomial_evaluation(X*Y, P), !.
symbolic_dot_prod([X|Xs], [Y|Ys], R):-
	polynomial_evaluation(X*Y, P),
	symbolic_dot_prod(Xs, Ys, Q),
	polynomial_evaluation(P + Q, R).
