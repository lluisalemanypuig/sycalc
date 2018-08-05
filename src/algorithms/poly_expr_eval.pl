:-ensure_loaded("../core").
:-ensure_loaded("power_sums").

/***
	@descr This file contains useful predicates for the evaluation
	of monomials and polynomials. Composition and summation of monomials
	are two examples of such evaluation.
*/

/*! Monomial evaluation. */

/**
	@form monomial_composition(Var, Expr, Monomial, Result)
	@descr @Result is the replacement of variable @Var in @Monomial
	with expression @Expr. In other words, compose @Monomial with @Expr
	at variable @Var.
*/
monomial_composition(V,E, M, N):-
	monomial_split(V, M, Vo,Wo),
	monomial_var_exp(Vo, V, V^P),
	polynomial_expression_evaluation( Wo*(E^P), N ).

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

	polynomial_composition(n,F, PS, EPlus),

	polynomial_expression_evaluation(I-1, I1),
	polynomial_composition(n,I1, PS, EMinus),

	polynomial_expression_evaluation(Wo*(EPlus - EMinus), S).

/**
	@form monomial_summation(Var, Ini,Fin, Monomial, Sum)
	@descr @Sum is the result of the algebraic summation of @Monomial
	on variable @Var from @Ini to @Fin.
	
	Examples:
	<++
	!> If we want to calculate the sum of all the values a monomial
	can take on a certain variable:
	<--
	?- monomial_summation(i, 0,10, i, S).
	S = 55.
	-->
	because
	<--
	\sum_{i=0}^{10} i = 55
	-->
	!> If we want to calculate the sum of all the values a monomial
	can take on a variable not in the monomial:
	<--
	?- monomial_summation(j, 0,10, i, S).
	S = 11*i.
	-->
	because
	<--
	\sum_{j=0}^{10} i = 11*i
	-->
	++>
*/
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

/*! Polynomial evaluation. */

/**
	@form polynomial_composition(Var, Expr, Poly, Result)
	@descr @Result if the composition of all monomials of polynomial
	@Poly at variable @Var with expression @Expr. Uses predicate
	?monomial_composition/4.
*/
polynomial_composition(V,E, P, R):-
	list_from_polynomial(P, MS),
	map(monomial_composition(V,E), MS, MSR),
	foldl(polynomial_expression_sum, 0, MSR, Q),
	polynomial_expression_evaluation(Q, R).

/**
	@form polynomial_expression_evaluation_list(Expr, List)
	@descr @List is the list of monomials that results from evaluating
	the polynomial expression @Expr.
	@constrs @Expr is either a monomial or a polynomial expression of the
	form:
	<--
	A + B
	A - B
	A*B
	(-A)^N
	A^N
	-A
	choose(A, N)
	-->
	where both A and B are polynomial expressions and N an integer value.
	
	choose(A,N) represents the binomial \binom{A}{N}. For example:
	<--
	?- polynomial_expression_evaluation_list( choose(n,2), R ).
	R = 1/2*n^2 - 1/2*n.
	-->
*/
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

/**
	@form polynomial_expression_evaluation(Expr, Poly)
	@descr @Poly is the reduced polynomial that results from evaluating
	the polynomial expression @Expr. Uses predicate
	?polynomial_expression_evaluation_list/2.
	@constrs Constraints as in predicate ?polynomial_expression_evaluation_list/2.
*/
polynomial_expression_evaluation(P, R):-
	polynomial_expression_evaluation_list(P, L), polynomial_from_list(L, R).

/**
	@form polynomial_expression_sum(P, Q, R)
	@descr @R equals @P+@Q.
*/
polynomial_expression_sum(P, Q, R):-
	polynomial_expression_evaluation_list(P+Q, L), polynomial_from_list(L, R).
/**
	@form polynomial_expression_sub(P, Q, R)
	@descr @R equals @P-@Q.
*/
polynomial_expression_sub(P, Q, R):-
	polynomial_expression_evaluation_list(P-Q, L), polynomial_from_list(L, R).
/**
	@form polynomial_expression_prod(P, Q, R)
	@descr @R equals @P*@Q.
*/
polynomial_expression_prod(P, Q, R):-
	polynomial_expression_evaluation_list(P*Q, L), polynomial_from_list(L, R).

/*! Other useful predicates for polynomials. */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% SYMBOLIC DOT PRODUCT %%%%

/**
	@form symbolic_dot_prod(List1, List2, P)
	@descr @P is the dot product of the elements in @List1 and @List2.
	@constrs @List1 and @List2 may both contain polynomial expressions
	and must have the same, strictly positive length.
*/
symbolic_dot_prod([X], [Y], P):- polynomial_evaluation(X*Y, P), !.
symbolic_dot_prod([X|Xs], [Y|Ys], R):-
	polynomial_evaluation(X*Y, P),
	symbolic_dot_prod(Xs, Ys, Q),
	polynomial_evaluation(P + Q, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% OTHER POLYNOMIAL USEFUL OPERATIONS %%%%%%%%%

/**
	@form falling_factorial(Poly, N, FallingFactorial)
	@descr Computes the product:
	<--
	FallingFactorial = \prod_{i=0}^{N} (Poly - i)
	-->
	@constrs @Poly is a polynomial expression.
*/
falling_factorial(P, 1, FF):- polynomial_expression_evaluation(P, FF), !.
falling_factorial(P, I, FF):-
	I1 is I - 1,
	polynomial_expression_evaluation(P - 1, Pminus1),
	falling_factorial(Pminus1, I1, FF1),
	polynomial_expression_evaluation(P*FF1, FF).

/**
	@form polynomial_eval_eq(Expr1, Expr2)
	@descr This predicate fails if the polynomial expressions @Expr1 and
	@Expr2 are not equal.
*/
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
/**
	@form expanded_polynomial_evaluation(Value, Var, Poly, Res)
	@descr Evaluates all monomials in the polynomial @Poly at variable
	@Var with value @Value.
	@constrs
		@param Value Must be a rational value.
		@param Poly Must be an expanded polynomial.
*/
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
/**
	@form polynomial_value_evaluation(Value, Var, Poly, Res)
	@descr @Res is the result of replacing variable @Var in @Poly with
	value @Value. Uses predicate ?expanded_polynomial_evaluation/4.
*/
polynomial_value_evaluation(VAL, V, P, E):-
	polynomial_expression_evaluation(P, EXP),
	expanded_polynomial_evaluation(VAL, V, EXP, E).

