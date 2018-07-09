/***
	@descr This file contains basic integer-related predicates. Is a
	number even or odd? What is the greatest common divisor of two
	numbers, or among a list of numbers?
	
	All parameters must be integer, or contain integer values in case
	they are lists.
*/

/**
	@form multiple(A,B)
	@constraints A and B integer values.
	@descr Succeeds if, and only if, A is a multiple of B, that is,
	if B divides A evenly.
*/
multiple(_, 0):- !, false.
multiple(_, 1):- !, true.
multiple(A, A):- !, true.
multiple(A, B):- D is A mod B, D == 0, !.
multiple(_, _):- false.

/**
	@form even(A)
	@constraints A integer value.
	@descr Predicate fails if A is odd.
*/
even(A):- B is A mod 2, B == 0.

/**
	@form odd(A)
	@constraints A integer value.
	@descr Predicate fails if A is even.
*/
odd(A):- B is A mod 2, B == 1.

/**
	@form gcd(A,B, G)
	@constraints A and B integer values.
	@descr G is the greatest common divisor of two integers A and B.
*/
gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, G):- X < 0, Xp is -X, gcd(Xp, Y, G), !.
gcd(X, Y, G):- Y < 0, Yp is -Y, gcd(X, Yp, G), !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).

/**
	@form gcd_rel(A,B, G, C,D)
	@constraints A and B integer values.
	@descr G is the greatest common divisor of two integers A and B.
	C is the result of dividing A by G, and D of dividing D by G.
*/
gcd_rel(A, B, G, C, D):- gcd(A, B, G), C is A/G, D is B/G, !.

/**
	@form gcd_list(List, G)
	@constraints List cannot be empty and can only contain integer values.
	@descr G is the greatest common divisor of all the integer values in
*/
gcd_list([X], X):- X > 0, !.
gcd_list([X], Xp):- Xp is -X, !.
gcd_list([X|L], G):- gcd_list(L, G1), gcd(X, G1, G).

% Auxiliar predicate for divisors predicate
divisorsp(X, X, [X]):- !.
divisorsp(X, S, [S|K]):- S =< X, multiple(X, S), S1 is S + 1, divisorsp(X, S1, K), !.
divisorsp(X, S, K):- S1 is S + 1, divisorsp(X, S1, K), !.

% Auxiliar predicate for divisors predicate
divisorsn(X, Y, [Y]):- X is -Y, !.
divisorsn(X, S, [S|K]):- S =< X, multiple(X, S), S1 is S - 1, divisorsn(X, S1, K), !.
divisorsn(X, S, K):- S1 is S - 1, divisorsn(X, S1, K), !.

% Compare two numbers both divisors of the same number
compare_divisors(X, Y):- X < 0, Y < 0, abs(X, AX), abs(Y, AY), AX < AY.
compare_divisors(X, Y):- X > 0, Y > 0, X < Y.
compare_divisors(X, Y):- X < 0, Y > 0, X is -Y.
compare_divisors(X, Y):- X < 0, Y > 0, true.
compare_divisors(X, Y):- X > 0, Y < 0, X is -Y.
compare_divisors(X, Y):- X > 0, Y < 0, false.

% Find all the divisors of a number (negative divisors included)
/**
	@form divisors(A, Divisors)
	@constraints A integer value.
	@descr Divisors contains all the divisors of A sorted by their
	absolute value, breaking ties by considering that positive divisors
	go before negative values. The divisors of 30, for instance, are:
		[1,-1,2,-2,3,-3,5,-5,6,-6,10,-10,15,-15,30,-30]
*/
divisors(1, [1,-1]):- !.
divisors(-1, [1,-1]):- !.
divisors(X, [1,-1|S]):-
	X < 0, !, XX is -X, divisorsp(XX, 2, P), divisorsn(XX, -2, N),
	list_concat(P, N, C), isort_by(compare_divisors, C, S).
divisors(X, [1,-1|S]):-
	divisorsp(X, 2, P), divisorsn(X, -2, N), list_concat(P, N, C),
	isort_by(compare_divisors, C, S).
