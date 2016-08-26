% LIST-RELATED FUNCTIONS

min([X], X).
min([X|L], M):- min(L,N), X > N, M is N, !.
min([X|_], X).

max([X], X).
max([X|L], M):- min(L,N), X < N, M is N, !.
max([X|_], X).
