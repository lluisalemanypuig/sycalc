:-ensure_loaded(polynomial).
:-ensure_loaded(list).

% Takes a polynomial as a list of monomials and reduces it
list_red_monomials([], []):- !.
list_red_monomials([0], []):- !.
list_red_monomials([M], [RM]):- red_monomial(M, RM), !.
list_red_monomials([M1,M2], []):- mon_sum(M1, M2, 0), !.
list_red_monomials([M1,M2], [S]):- mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)), !.
list_red_monomials([M1,M2], [M1,M2]):- !.
list_red_monomials([M1,M2|L], R):- mon_sum(M1, M2, S), not(polynomial_eq(M1 + M2, S)), list_red_monomials([S|L], R), !.
list_red_monomials([M1,M2|L], [M1|R]):- list_red_monomials([M2|L], R), !.
list_red_monomials(X, X).

% Takes a polynomial as a list and returns it as a reduced list of monomials
% These are not reduced lists of polynomials:
% - [x^2, -2*x^2, x]
% - [x^3, 0, 1]
% These are reduced lists of polynomials:
% - [x^3, 1]
% - [x^3, 4*x^2, 1]
list_red_list_polynomial(L, LR):- monomial_sort(L, R), list_red_monomials(R, LR).

%%%%%%%%%%%%%
%%%% SUM %%%%

% Takes an expanded polynomial and reduces it: x + x + 2 -> 2*x + 2
polynomial_sum(P, R):- polynomial_monomials(P, M), list_red_list_polynomial(M, L), list_polynomial(L, R).

% Takes two polynomials each of them as reduced and decreasingly sorted lists of
% monomials of unique degree and adds the second from to first and returns it as
% a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
list_polynomial_sum_sorted_list__(P1, D1, P2, D2, R):- D1 < D2,
	NZEROES is D2 - D1, padded_list_begin(P1, NZEROES, 0, PP1),
	zip_with(mon_sum, PP1, P2, R).
list_polynomial_sum_sorted_list__(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2, padded_list_begin(P2, NZEROES, 0, PP2),
	zip_with(mon_sum, P1, PP2, R).

list_polynomial_sum_sorted_list_(P, [], P):- !.
list_polynomial_sum_sorted_list_([], P, P):- !.
list_polynomial_sum_sorted_list_(PP1, PP2, R):-
	first(PP1, FMON1, _), first(PP2, FMON2, _),
	monomial_degree(FMON1, D1), monomial_degree(FMON2, D2),
	list_polynomial_sum_sorted_list__(PP1, D1, PP2, D2, R).

list_polynomial_sum_sorted_list(P, [], P):- !.
list_polynomial_sum_sorted_list([], P, P):- !.
list_polynomial_sum_sorted_list(P1, P2, R):-
	padded_poly_mons_decr(P1, PP1), padded_poly_mons_decr(P2, PP2),
	list_polynomial_sum_sorted_list_(PP1, PP2, LR),
	list_red_monomials(LR, R), !.

% Takes two polynomials each of them as a list, adds the second from the first
% and returns it as a reduced list of monomials
list_polynomial_sum_list(P1, P2, R):- concat(P1, P2, P), list_red_list_polynomial(P, R).

%%%%%%%%%%%%%
%%%% SUB %%%%

% Takes two polynomials each of them as reduced and DECREASINGLY sorted lists of
% monomials of unique degree and substracts the second from the first and returns
% it as a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
list_polynomial_sub_sorted_list_(P1, D1, P2, D2, R):- D1 < D2,
	NZEROES is D2 - D1, padded_list_begin(P1, NZEROES, 0, PP1),
	zip_with(mon_sub, PP1, P2, R).
list_polynomial_sub_sorted_list_(P1, D1, P2, D2, R):-
	NZEROES is D1 - D2, padded_list_begin(P2, NZEROES, 0, PP2),
	zip_with(mon_sub, P1, PP2, R).

%list_polynomial_sub_sorted_list([1/4*n^5,5/8*n^4,1/2*n^3,1/8*n^2], [1/12*n^3,1/8*n^2,1/24*n], R).

list_polynomial_sub_sorted_list(P1, [], P1):- !.
list_polynomial_sub_sorted_list([], P2, R):- map(monomial_neg, P2, R), !.
list_polynomial_sub_sorted_list(P1, P2, R):-
	padded_poly_mons_decr(P1, PP1), padded_poly_mons_decr(P2, PP2),
	first(PP1, FMON1, _), first(PP2, FMON2, _),
	monomial_degree(FMON1, D1), monomial_degree(FMON2, D2),
	list_polynomial_sub_sorted_list_(PP1, D1, PP2, D2, LR),
	list_red_monomials(LR, R), !.

% Takes two polynomials each of them as reduced and DECREASINGLY sorted lists of
% monomials of unique degree and multiplies the second and the first and returns
% it as a reduced list of monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
list_polynomial_sub_list(P1, P2, R):-
	map(monomial_neg, P2, NP2), list_polynomial_sum_list(P1, NP2, R).

%%%%%%%%%%%%%%
%%%% PROD %%%%

% Takes two polynomials each of them as reduced and DECREASINGLY sorted lists of
% monomials of unique degree and multiplies them. The result is a list of reduced
% monomials.
% This list has monomials with no unique degree: [2*x^2, x^2, x, 1]
% This list has monomials with unique degree: [3*x^2, x, 1]
list_polynomial_prod_sorted_list_([], _, [[]]):- !.
list_polynomial_prod_sorted_list_(_, [], [[]]):- !.
list_polynomial_prod_sorted_list_([M], L2, [P]):-
	map(mon_prod(M), L2, P2),
	padded_poly_mons_decr(P2, P), !.
list_polynomial_prod_sorted_list_([M|L], L2, [P|Q]):-
	map(mon_prod(M), L2, P2), padded_poly_mons_decr(P2, P),
	list_polynomial_prod_sorted_list_(L, L2, Q), !.

list_polynomial_prod_sorted_list(L1, L2, L):-
	list_polynomial_prod_sorted_list_(L1, L2, ML),
	foldl(list_polynomial_sum_sorted_list_, [], ML, LR),
	list_red_monomials(LR, L), !.

% Takes two polynomials each of them as a list, multiplies them and returns it as
% a reduced list of monomials
list_polynomial_prod_list(L1, L2, L):-
	cartesian_product_by(mon_prod, L1, L2, MON_PROD),
	list_red_list_polynomial(MON_PROD, L).

% Takes two expanded polynomials and multiplies them
% polynomial_prod(P, Q, R), where R = P*Q
polynomial_prod(P1, P2, P):-
	polynomial_monomials(P1, L1), polynomial_monomials(P2, L2),
	list_polynomial_prod_list(L1, L2, MON_PROD), list_polynomial(MON_PROD, P).

%%%%%%%%%%%%%%%
%%%% POWER %%%%

% Takes a polynomial as a list, an integer number and performs the power P^N
list_polynomial_power_list(_, 0, [1]):- !.
list_polynomial_power_list(L, 1, L):- !.
list_polynomial_power_list(LP, N, LN):-
	natural(N), N1 is N - 1, list_polynomial_power_list(LP, N1, L),
	list_polynomial_prod_list(LP, L, LN).

% Takes an expanded polynomial, an integer number and performs the power P^N
% polynomial_prod(P, N, Q), where Q = P^N
polynomial_power(_, 0, 1):- !.
polynomial_power(P, 1, P):- !.
polynomial_power(P, N, PN):-
	polynomial_monomials(P, M), list_polynomial_power_list(M, N, L),
	list_polynomial(L, PN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% POLYNOMIAL EXPRESSIONS' EVALUATION %%%%

polynomial_evaluation_list(Q1 + Q2, R):-
	polynomial_evaluation_list(Q1, L1), polynomial_evaluation_list(Q2, L2),
	concat(L1, L2, L), list_red_list_polynomial(L, R), !.
polynomial_evaluation_list(Q1 - Q2, R):-
	polynomial_evaluation_list(Q1, L1), polynomial_evaluation_list(Q2, L2),
	map(monomial_neg, L2, NL2),
	concat(L1, NL2, L), list_red_list_polynomial(L, R), !.
polynomial_evaluation_list(Q1 * Q2, R):-
	polynomial_evaluation_list(Q1, L1), polynomial_evaluation_list(Q2, L2),
	cartesian_product(L1, L2, L), map(mon_prod, L, PROD), list_red_list_polynomial(PROD, R), !.
polynomial_evaluation_list(Q1 ^ N, R):-
	polynomial_evaluation_list(Q1, L1), list_polynomial_power_list(L1, N, R), !.
polynomial_evaluation_list(P, [R]):- red_monomial(P, R), !.

% Takes a sequence of sums and substractions P of polynomials, contracted or expanded,
% and operates it.
polynomial_evaluation(P, R):- polynomial_evaluation_list(P, L), list_polynomial(L, R).

% Takes two polynomials, expanded or contracted, evaluates them, and fails if they are not equal
polynomial_eval_eq(P1, P2):- polynomial_evaluation(P1, EP1), polynomial_evaluation(P2, EP2), polynomial_eq(EP1, EP2).

% POLYNOMIAL EVALUATION

% Takes an expanded polynomial and evaluates it with the value VAL
% VAL: real value
% P(x): expanded polynomial
% E: P(VAL)
expanded_polynomial_evaluation(VAL, P, E):-
	polynomial_monomials(P, MS), map(monomial_evaluation(VAL), MS, R), foldl(eval_sum, 0, R, E).

% Takes a contracted polynomial and evaluates it with the value VAL
contracted_polynomial_evaluation(VAL, P, E):-
	polynomial_evaluation(P, EXP), expanded_polynomial_evaluation(VAL, EXP, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% SYMBOLIC DOT PRODUCT %%%%

symbolic_dot_prod([X], [Y], P):- polynomial_evaluation( X*Y, P ), !.
symbolic_dot_prod([X|Xs], [Y|Ys], R):-
	polynomial_evaluation( X*Y, P ),
	symbolic_dot_prod(Xs, Ys, Q),
	polynomial_evaluation( P + Q, R ),
	!.



