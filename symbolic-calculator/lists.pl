% LIST-RELATED FUNCTIONS

min([X], X).
min([X|L], M):- min(L, N), X > N, !, M is N.
min([X|_], X).

max([X], X).
max([X|L], M):- max(L, N), X < N, !, M is N.
max([X|_], X).

first([X], X, []).
first([X|L], X, L).

last([X], [], X):- !.
last([X|R], [X|K], L):- last(R, K, L), !.

% replaces the first element of the list with Y
replace_first([_], Y, [Y]).
replace_first([_|L], Y, [Y|L]).

% High order functions

map(_, [], []).
map(F, [X|L], [E|R]):- call(F, X, E), map(F, L, R), !.

zip([A], [B], [(A, B)]).
zip([A|L], [B|R], [(A, B)|S]):- zip(L, R, S), !.

zipWith(F, [], [], X):- call(F, [], [], X), !.
zipWith(F, [A|L], [B|R], [C|S]):- call(F, A, B, C), zipWith(L, R, S), !.

concat([], L, L).
concat([A|L], R, [A|C]):- concat(L, R, C).

cartesian_product([], _, []).
cartesian_product(_, [], []).
cartesian_product([X], [Y|R], [[X,Y]|S]):- cartesian_product([X], R, S), !.
cartesian_product([X|L], R, S):- cartesian_product([X], R, S1), cartesian_product(L, R, S2), concat(S1, S2, S), !.

% SORTING ALGORITHMS

lt__(X, Y):- X < Y.

insert_by(_, X, [], [X]).
insert_by(F, X, [Y|L], [Y|R]):- not(call(F, X, Y)), insert_by(F, X, L, R), !.
insert_by(_, X, [Y|L], [X,Y|L]).

% insertion sort

isort_by(_, [], []).
isort_by(_, [X], [X]):- !.
isort_by(F, [X|L], R):- isort_by(F, L, S), !, insert_by(F, X, S, R).

insert(X, L, R):- insert_by(lt__, X, L, R).
isort(L, R):- isort_by(lt__, L, R).
