:-ensure_loaded("../core").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% POLYNOMIAL SUMMATION %%%%

% Computes the result of the summation over the induction variable VAR
% from the value INI to either the polynomial or value FIN of the
% polynomial P. Result in Q.
summation_over_polynomial(VAR, INI, FIN, P, Q):-
	false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% POLYNOMIAL EXPRESSIONS' EVALUATION %%%%

polynomial_evaluation_list(P, [R]):-
	monomial(P), red_monomial(P, R), !.
	
polynomial_evaluation_list(Q1 + Q2, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_evaluation_list(Q2, L2),
	concat(L1, L2, L),
	list_red_polynomial_from_list(L, R), !.
	
polynomial_evaluation_list(Q1 - Q2, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_evaluation_list(Q2, L2),
	map(monomial_neg, L2, NL2),
	concat(L1, NL2, L),
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
	
% Evaluation a summation over polynomials
% VAR: induction variable
% INI: lowest value of summation range. Must evaluate to a natural
% number
% FIN: highest value of summation range. Either a polynomial or an
% arithmetic expression
% EXPR: expression within the sum. A polynomial in
% any variable.
polynomial_evaluation_list(sum(VAR, INI, FIN, EXPR), R):-
	polynomial_evaluation_list(EXPR, P_EXPR),
	polynomial_evaluation_list(FIN, P_FIN),
	write(P_EXPR), nl,
	write(P_FIN), nl,
	false.
