% (INTEGER)-NUMERICAL ALGORITHMS

% A is multiple of B -- B divides A
% 4 is a multiple of 2
multiple(_, 0):- !, false.
multiple(_, 1):- !, true.
multiple(A, A):- !, true.
multiple(A, B):- D is A mod B, D == 0, !.
multiple(_, _):- false.

% Greatest common divisor
gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, G):- X < 0, Xp is -X, gcd(Xp, Y, G), !.
gcd(X, Y, G):- Y < 0, Yp is -Y, gcd(X, Yp, G), !.
gcd(X, Y, D):- X > Y, !, Z is X mod Y, gcd(Y, Z, D).
gcd(X, Y, D):- Z is Y mod X, gcd(X, Z, D).

gcd_rel(A, B, G, C, D):- gcd(A, B, G), C is A/G, D is B/G, !.

% gcd_list(L, G): G is the greatest common divisor of all the integers in L
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
divisors(1, [1,-1]):- !.
divisors(-1, [1,-1]):- !.
divisors(X, [1,-1|S]):-
	X < 0, !, XX is -X, divisorsp(XX, 2, P), divisorsn(XX, -2, N),
	concat(P, N, C), isort_by(compare_divisors, C, S).
divisors(X, [1,-1|S]):-
	divisorsp(X, 2, P), divisorsn(X, -2, N), concat(P, N, C),
	isort_by(compare_divisors, C, S).

% Compute the factorial of a number
factorial(0, 1):- !.
factorial(N, F):- N1 is N - 1, factorial(N1, F1), F is N*F1.
