:-ensure_loaded(polynomial).
:-ensure_loaded(list).

% Takes a polynomial as a list of monomials and reduces it
list_red_monomials([], []):- !.
list_red_monomials([0], []):- !.
list_red_monomials([M], [RM]):- red_monomial(M, RM), !.
list_red_monomials([M1,M2], []):- mon_sum(M1, M2, 0), !.
list_red_monomials([M1,M2], [S]):-
	mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)), !.
list_red_monomials([M1,M2], [M1,M2]):- !.
list_red_monomials([M1,M2|L], R):-
	mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)),
	list_red_monomials([S|L], R), !.
list_red_monomials([M1,M2|L], [M1|R]):- list_red_monomials([M2|L], R), !.
list_red_monomials(X, X).

% Takes a polynomial as a list and returns it as a reduced list of monomials
% These are not reduced lists of polynomials:
% - [x^2, -2*x^2, x]
% - [x^3, 0, 1]
% These are reduced lists of polynomials:
% - [x^3, 1]
% - [x^3, 4*x^2, 1]
list_red_polynomial_from_list(L, LR):-
	monomial_sort(L, R), list_red_monomials(R, LR).

%%%%%%%%%%%%%
%%%% SUM %%%%

% Takes an expanded polynomial and reduces it: x + x + 2 -> 2*x + 2
polynomial_sum(P, R):-
	list_from_polynomial(P, M), list_red_polynomial_from_list(M, L),
	polynomial_from_list(L, R).

% Takes two polynomials each of them as reduced and decreasingly sorted
% lists of monomials of unique degree and adds the second from to first
% and returns it as a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
polynomial_from_list_sum_sorted_list__(P1, D1, P2, D2, R):- D1 < D2,
	NZEROES is D2 - D1, padded_list_begin(P1, NZEROES, 0, PP1),
	zip_with(mon_sum, PP1, P2, R).
polynomial_from_list_sum_sorted_list__(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2, padded_list_begin(P2, NZEROES, 0, PP2),
	zip_with(mon_sum, P1, PP2, R).

polynomial_from_list_sum_sorted_list_(P, [], P):- !.
polynomial_from_list_sum_sorted_list_([], P, P):- !.
polynomial_from_list_sum_sorted_list_(PP1, PP2, R):-
	first(PP1, FMON1, _), first(PP2, FMON2, _),
	monomial_degree(FMON1, D1), monomial_degree(FMON2, D2),
	polynomial_from_list_sum_sorted_list__(PP1, D1, PP2, D2, R).

polynomial_from_list_sum_sorted_list(P, [], P):- !.
polynomial_from_list_sum_sorted_list([], P, P):- !.
polynomial_from_list_sum_sorted_list(P1, P2, R):-
	padded_poly_mons_decr(P1, PP1), padded_poly_mons_decr(P2, PP2),
	polynomial_from_list_sum_sorted_list_(PP1, PP2, LR),
	list_red_monomials(LR, R), !.

% Takes two polynomials each of them as a list, adds the second from the
% first and returns it as a reduced list of monomials
polynomial_from_list_sum_list(P1, P2, R):-
	concat(P1, P2, P), list_red_polynomial_from_list(P, R).

%%%%%%%%%%%%%
%%%% SUB %%%%

% Takes two polynomials each of them as reduced and DECREASINGLY sorted
% lists of monomials of unique degree and substracts the second from the
% first and returns it as a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
polynomial_from_list_sub_sorted_list_(P1, D1, P2, D2, R):- D1 < D2,
	NZEROES is D2 - D1, padded_list_begin(P1, NZEROES, 0, PP1),
	zip_with(mon_sub, PP1, P2, R).
polynomial_from_list_sub_sorted_list_(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2, padded_list_begin(P2, NZEROES, 0, PP2),
	zip_with(mon_sub, P1, PP2, R).

polynomial_from_list_sub_sorted_list(P1, [], P1):- !.
polynomial_from_list_sub_sorted_list([], P2, R):- map(monomial_neg, P2, R), !.
polynomial_from_list_sub_sorted_list(P1, P2, R):-
	padded_poly_mons_decr(P1, PP1), padded_poly_mons_decr(P2, PP2),
	first(PP1, FMON1, _), first(PP2, FMON2, _),
	monomial_degree(FMON1, D1), monomial_degree(FMON2, D2),
	polynomial_from_list_sub_sorted_list_(PP1, D1, PP2, D2, LR),
	list_red_monomials(LR, R), !.

% Takes two polynomials each of them as reduced and DECREASINGLY sorted
% lists of monomials of unique degree and multiplies the second and the
% first and returns it as a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
polynomial_from_list_sub_list(P1, P2, R):-
	map(monomial_neg, P2, NP2), polynomial_from_list_sum_list(P1, NP2, R).

%%%%%%%%%%%%%%
%%%% PROD %%%%

% Takes two polynomials each of them as reduced and DECREASINGLY sorted
% lists of monomials of unique degree and multiplies them. The result is
% a list of reduced monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
polynomial_from_list_prod_sorted_list_([], _, [[]]):- !.
polynomial_from_list_prod_sorted_list_(_, [], [[]]):- !.
polynomial_from_list_prod_sorted_list_([M], L2, [P]):-
	map(mon_prod(M), L2, P2),
	padded_poly_mons_decr(P2, P), !.
polynomial_from_list_prod_sorted_list_([M|L], L2, [P|Q]):-
	map(mon_prod(M), L2, P2), padded_poly_mons_decr(P2, P),
	polynomial_from_list_prod_sorted_list_(L, L2, Q), !.
polynomial_from_list_prod_sorted_list(L1, L2, L):-
	polynomial_from_list_prod_sorted_list_(L1, L2, ML),
	foldl(polynomial_from_list_sum_sorted_list_, [], ML, LR),
	list_red_monomials(LR, L), !.

% Takes two polynomials each of them as a list, multiplies them and
% returns it as a reduced list of monomials
polynomial_from_list_prod_list(L1, L2, L):-
	cartesian_product_by(mon_prod, L1, L2, MON_PROD),
	list_red_polynomial_from_list(MON_PROD, L).

% Takes two expanded polynomials and multiplies them
% polynomial_prod(P, Q, R), where R = P*Q
polynomial_prod(P1, P2, P):-
	list_from_polynomial(P1, L1), list_from_polynomial(P2, L2),
	polynomial_from_list_prod_list(L1, L2, MON_PROD),
	polynomial_from_list(MON_PROD, P).

%%%%%%%%%%%%%%%
%%%% POWER %%%%

% Takes a polynomial as list, a natural number and computes P^N
polynomial_from_list_power_list(_, 0, [1]):- !.
polynomial_from_list_power_list(L, 1, L):- !.
polynomial_from_list_power_list(LP, N, LN):-
	natural(N), even(N),
	Nhalf is N/2, polynomial_from_list_power_list(LP, Nhalf, L),
	polynomial_from_list_prod_list(L, L, LN), !.
polynomial_from_list_power_list(LP, N, LN):-
	natural(N), odd(N),
	N1 is N - 1, NmHalf is N1/2,
	polynomial_from_list_power_list(LP, NmHalf, L),
	polynomial_from_list_prod_list(L, L, LN1),
	polynomial_from_list_prod_list(LN1, LP, LN), !.

% Takes an expanded polynomial, an integer number and performs the power P^N
% polynomial_prod(P, N, Q), where Q = P^N
polynomial_power(_, 0, 1):- !.
polynomial_power(P, 1, P):- !.
polynomial_power(P, N, PN):-
	list_from_polynomial(P, M), polynomial_from_list_power_list(M, N, L),
	polynomial_from_list(L, PN).

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
polynomial_evaluation_list(Q1 * Q2, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_evaluation_list(Q2, L2),
	cartesian_product(L1, L2, L),
	map(mon_prod, L, PROD),
	list_red_polynomial_from_list(PROD, R), !.
polynomial_evaluation_list(Q1 ^ N, R):-
	polynomial_evaluation_list(Q1, L1),
	polynomial_from_list_power_list(L1, N, R), !.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% OTHER POLYNOMIAL USEFUL OPERATIONS %%%%%%%%%

% Constructs the falling factorial polynomial on polynomial P:
% (P - I)*(P - (I - 1))*(P - (I - 2))* .... *(P - 1)*P
falling_factorial(P, 1, FF):- polynomial_evaluation(P, FF), !.
falling_factorial(P, I, FF):-
	I1 is I - 1,
	polynomial_evaluation(P - 1, Pminus1),
	falling_factorial(Pminus1, I1, FF1),
	polynomial_evaluation(P*FF1, FF).

% Takes a sequence of sums and substractions P of polynomials, contracted
% or expanded, and operates it.
polynomial_evaluation(P, R):-
	polynomial_evaluation_list(P, L), polynomial_from_list(L, R).

% Takes two polynomials, expanded or contracted, evaluates them, and
% fails if they are not equal
polynomial_eval_eq(P1, P2):-
	polynomial_evaluation(P1, EP1), polynomial_evaluation(P2, EP2),
	polynomial_eq(EP1, EP2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% POLYNOMIAL EVALUATION %%%%%%%%%

% Takes an expanded polynomial and evaluates it with the value VAL
% VAL: real value
% P(x): expanded polynomial
% E: P(VAL)
expanded_polynomial_evaluation(VAL, P, E):-
	list_from_polynomial(P, MS), map(monomial_evaluation(VAL), MS, R),
	foldl(eval_sum, 0, R, E).

% Takes a contracted polynomial and evaluates it with the value VAL
% VAL: real value
% P(x): expanded polynomial
% E: P(VAL)
polynomial_evaluation(VAL, P, E):-
	polynomial_evaluation(P, EXP),
	expanded_polynomial_evaluation(VAL, EXP, E).

% Takes an expanded

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% SYMBOLIC DOT PRODUCT %%%%

% Takes two lists of polynomials and computes the dot product of the two
% lists.
symbolic_dot_prod([X], [Y], P):- polynomial_evaluation(X*Y, P), !.
symbolic_dot_prod([X|Xs], [Y|Ys], R):-
	polynomial_evaluation(X*Y, P),
	symbolic_dot_prod(Xs, Ys, Q),
	polynomial_evaluation(P + Q, R).



