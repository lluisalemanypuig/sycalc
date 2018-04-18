:-ensure_loaded(polynomial).
:-ensure_loaded(list).

% Takes a multivariate polynomial as a list of monomials and reduces it
list_red_monomials([], []):- !.
list_red_monomials([0], []):- !.
list_red_monomials([M], [RM]):- red_monomial(M, RM), !.
list_red_monomials([M1,M2], []):- mon_sum(M1, M2, R), R = 0, !.
list_red_monomials([M1,M2], [S]):- mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)), !.
list_red_monomials([M1,M2], [M1,M2]):- !.
list_red_monomials([M1,M2|L], R):-
	mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)),
	list_red_monomials([S|L], R), !.
list_red_monomials([M1,M2|L], [M1|R]):- list_red_monomials([M2|L], R), !.
list_red_monomials(X, X).

% Takes a multivariate polynomial as a list and returns it as a reduced
% list of monomials.
% These are not reduced lists of polynomials:
% - [x^2,-2*x^2, x, y]
% - [x^3, 0, 1]
% These are reduced lists of polynomials:
% - [x^3, 1]
% - [x^3, 4*x^2, 1]
list_red_polynomial_from_list(L, LR):-
	monomial_sort(L, R), list_red_monomials(R, LR).

%%	ATTENTION:
%%	All arithmetic operations between polynomials as lists of
%%	monomials assume both polynomials to have the same single
%%	variable. Therefore, the two polynomials must be univariate.

%---------------
%%	SUM

% Takes an expanded multivariate polynomial and reduces it:
% x + x + 2 -> 2*x + 2
polynomial_reduction(P, R):-
	list_from_polynomial(P, M),
	list_red_polynomial_from_list(M, L),
	polynomial_from_list(L, R).

% Takes two uni-variate polynomials each of them as reduced and
% decreasingly sorted lists of monomials of unique degree and adds the
% second from to first and returns it as a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
unipoly_from_list_sum_sorted_list__(P1, D1, P2, D2, R):- D1 < D2, !,
	NZEROES is D2 - D1,
	padded_list_begin(NZEROES, P1, 0, PP1),
	zip_with(mon_sum, PP1, P2, R).
unipoly_from_list_sum_sorted_list__(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2,
	padded_list_begin(NZEROES, P2, 0, PP2),
	zip_with(mon_sum, P1, PP2, R).

unipoly_from_list_sum_sorted_list_(PP1, PP2, R):-
	first(PP1, M1, _),
	first(PP2, M2, _),
	unimonomial_degree(M1, D1),
	unimonomial_degree(M2, D2),
	unipoly_from_list_sum_sorted_list__(PP1, D1, PP2, D2, R).

unipoly_from_list_sum_sorted_list(P, [], P):- !.
unipoly_from_list_sum_sorted_list([], P, P):- !.
unipoly_from_list_sum_sorted_list(P1, P2, R):-
	padded_unipoly_mons_decr(P1, PP1),
	padded_unipoly_mons_decr(P2, PP2),
	unipoly_from_list_sum_sorted_list_(PP1, PP2, LR),
	list_red_monomials(LR, R), !.

% Takes two polynomials each of them as a list, adds the second from the
% first and returns it as a reduced list of monomials
polynomial_from_list_sum_list(P1, P2, R):-
	list_concat(P1, P2, P),
	list_red_polynomial_from_list(P, R).

%---------------
%%	SUB

% Takes two uni-variate polynomials each of them as reduced and
% DECREASINGLY sorted lists of monomials of unique degree and substracts
% the second from the first and returns it as a reduced list of
% monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
unipoly_from_list_sub_sorted_list(P1, D1, P2, D2, R):- D1 < D2,
	NZEROES is D2 - D1,
	padded_list_begin(NZEROES, P1, 0, PP1),
	zip_with(mon_sub, PP1, P2, R), !.
unipoly_from_list_sub_sorted_list(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2,
	padded_list_begin(NZEROES, P2, 0, PP2),
	zip_with(mon_sub, P1, PP2, R).

unipoly_from_list_sub_sorted_list(P1, [], P1):- !.
unipoly_from_list_sub_sorted_list([], P2, R):- map(monomial_neg, P2, R), !.
unipoly_from_list_sub_sorted_list(P1, P2, R):-
	padded_unipoly_mons_decr(P1, PP1),
	padded_unipoly_mons_decr(P2, PP2),
	first(PP1, FMON1, _), first(PP2, FMON2, _),
	unimonomial_degree(FMON1, D1),
	unimonomial_degree(FMON2, D2),
	unipoly_from_list_sub_sorted_list(PP1, D1, PP2, D2, LR),
	list_red_monomials(LR, R), !.

% Takes two uni-variate polynomials each of them as reduced and
% DECREASINGLY sorted lists of monomials of unique degree and multiplies
% the second and the first and returns it as a reduced list of
% monomials.
% This list has monomials with no unique degree: [2*x^2,x^2,x,1]
% This list has monomials with unique degree: [3*x^2, x, 1]
polynomial_from_list_sub_list(P1, P2, R):-
	map(monomial_neg, P2, NP2),
	polynomial_from_list_sum_list(P1, NP2, R).

%---------------
%%	PROD

% Takes two uni-variate polynomials each of them as reduced and
% DECREASINGLY sorted lists of monomials of unique degree and multiplies
% them. The result is a list of reduced monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
unipoly_from_list_prod_sorted_list_([], _, [[]]):- !.
unipoly_from_list_prod_sorted_list_(_, [], [[]]):- !.
unipoly_from_list_prod_sorted_list_([M], L2, [P]):-
	map(mon_prod(M), L2, P), !.
unipoly_from_list_prod_sorted_list_([M|L], L2, [P|Q]):-
	map(mon_prod(M), L2, P),
	unipoly_from_list_prod_sorted_list_(L, L2, Q), !.

unipoly_from_list_prod_sorted_list([], [], []):- !.
unipoly_from_list_prod_sorted_list([],  _, []):- !.
unipoly_from_list_prod_sorted_list( _, [], []):- !.
unipoly_from_list_prod_sorted_list(L1, L2, L):-
	unipoly_from_list_prod_sorted_list_(L1, L2, ML),
	foldl(unipoly_from_list_sum_sorted_list, [], ML, LR),
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

%---------------
%%	 POWER

% Takes a polynomial as list, a natural number and computes P^N
polynomial_from_list_power_list(_, 0, [1]):- !.
polynomial_from_list_power_list(L, 1, L):- !.
polynomial_from_list_power_list(LP, N, LN):-
	natural(N), even(N),
	Nhalf is N/2,
	polynomial_from_list_power_list(LP, Nhalf, L),
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
	list_from_polynomial(P, M),
	polynomial_from_list_power_list(M, N, L),
	polynomial_from_list(L, PN).

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

% Takes two polynomials, expanded or contracted, evaluates them, and
% fails if they are not equal
polynomial_eval_eq(P1, P2):-
	polynomial_evaluation(P1, EP1),
	polynomial_evaluation(P2, EP2),
	polynomial_eq(EP1, EP2).

% Takes an expanded polynomial and evaluates it with the value VAL
% on variable X. All those monomials with that variable will be
% evaluated on such variable.
% VAL: real value
% P(x): expanded polynomial
% E: P(VAL)
expanded_polynomial_evaluation(VAL, V, P, E):-
	list_from_polynomial(P, MS),
	map(monomial_evaluation(VAL, V), MS, R),
	list_red_polynomial_from_list(R, L),
	polynomial_from_list(L, E).

% Takes an expanded polynomial and evaluates it with the value VAL
% on variable X. All those monomials with that variable will be
% evaluated on such variable.
% VAL: real value
% P(x): expanded polynomial
% E: P(VAL)
polynomial_evaluation(VAL, V, P, E):-
	polynomial_evaluation(P, EXP),
	expanded_polynomial_evaluation(VAL, V, EXP, E).




