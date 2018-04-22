:-ensure_loaded("../core").

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
