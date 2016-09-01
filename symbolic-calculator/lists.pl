% LIST-RELATED FUNCTIONS

min([X], X).
min([X|L], M):- min(L, N), X > N, !, M is N.
min([X|_], X).

max([X], X).
max([X|L], M):- max(L, N), X < N, !, M is N.
max([X|_], X).

% High order functions

map(_, [], []).
map(F, [X|L], [E|R]):- call(F, X, E), map(F, L, R), !.

zip([A], [B], [(A, B)]).
zip([A|L], [B|R], [(A, B)|S]):- zip(L, R, S), !.

zipWith(_, [], [], []).
zipWith(F, [A|L], [B|R], [C|S]):- call(F, A, B, C), zipWith(L, R, S), !.

concat([], L, L).
concat([A|L], R, [A|C]):- concat(L, R, C).

% SORTING ALGORITHMS

% insertion sort

insert_by(_, X, [], [X]).
insert_by(F, X, [Y|L], [Y|R]):- not(call(F, X, Y)), insert_by(F, X, L, R), !.
insert_by(_, X, [Y|L], [X,Y|L]).

isort_by(_, [], []).
isort_by(_, [X], [X]):- !.
isort_by(F, [X|L], R):- isort_by(F, L, S), !, insert_by(F, X, S, R).

lt__(X, Y):- X < Y.
insert(X, L, R):- insert_by(lt__, X, L, R).
isort(L, R):- isort_by(lt__, L, R).
