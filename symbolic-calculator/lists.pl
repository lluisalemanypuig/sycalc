% LIST-RELATED FUNCTIONS

min([X], X).
min([X|L], M):- min(L, N), X > N, !, M is N.
min([X|_], X).

max([X], X).
max([X|L], M):- max(L, N), X < N, M is N, !.
max([X|_], X).

% High order functions

map(_, []], []).
map(F, [X|L], [E|R]):- call(F, X, E), map(F, L, R).

zip([A], [B], [(A, B)]).
zip([A|L], [B|R], [(A, B)|S]):- zip(L, R, S).

zipWith(_, [], [], []).
zipWith(F, [A|L], [B|R], [C|S]):- call(F, A, B, S), zipWith(L, R, S).


