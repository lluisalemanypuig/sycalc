% LIST-RELATED FUNCTIONS

min([X], X):- !.
min([X|L], M):- min(L, N), X > N, !, M is N.
min([X|_], X).

max([X], X):- !.
max([X|L], M):- max(L, N), X < N, !, M is N.
max([X|_], X).

first([X], X, []):- !.
first([X|L], X, L):- !.

last([X], [], X):- !.
last([X|R], [X|K], L):- last(R, K, L).

% replaces the first element of the list with Y
replace_first([_], Y, [Y]):- !.
replace_first([_|L], Y, [Y|L]).

% drops all elements equal to O from list
drop(_, [], []):- !.
drop(O, [X], [X]):- O \= X, !.
drop(O, [O|Xs], D):- drop(O, Xs, D), !.
drop(O, [X|Xs], [X|Ds]):- drop(O, Xs, Ds).

% replace all elements equal to O with I in list
drop_with(_, _, [], []):- !.
drop_with(O, _, [X], [X]):- O \= X, !.
drop_with(O, I, [O|Xs], [I|Ds]):- drop_with(O, I, Xs, Ds), !.
drop_with(O, I, [X|Xs], [X|Ds]):- drop_with(O, I, Xs, Ds).

% splits a list in two groups: the first containing all elements equal
% to X and the second containing the other elements
split_at(X,    [Y],    [Y],[]):- X == Y, !.
split_at(X,    [Y],    [],[Y]):- X \= Y, !.
split_at(X, [Y|Xx], [Y|Xs],Lr):- X == Y, split_at(X, Xx, Xs,Lr), !.
split_at(X, [Y|Yy], Xs,[Y|Lr]):- X \= Y, split_at(X, Yy, Xs,Lr).

% pairwise split
psplit_at(X,	   [Y],[Q],	  [Y],[Q],	   [],[]):- X == Y, !.
psplit_at(X,       [Y],[Q],         [],[],	 [Y],[Q]):- X \= Y, !.
psplit_at(X, [Y|Xx],[R|Rr], [X|Xs],[R|Rs],         Xr,Lr):- X == Y, psplit_at(X, Xx,Rr, Xs,Rs, Xr,Lr), !.
psplit_at(X, [Y|Xx],[R|Rr],	    Xs,Rs, [Y|Xr],[R|Lr]):- X \= Y, psplit_at(X, Xx,Rr, Xs,Rs, Xr,Lr).

% is element X in list?
member(X, [X]):- !.
member(X, [X|_]):- !.
member(X, [_|L]):- member(X, L).

% High order functions

% F :: a -> b
map(_, [], []):- !.
map(F, [X|L], [E|R]):- call(F, X, E), map(F, L, R), !.

% The inspection of a list with function F is the verification that
% applying the function F to all elements in the list never fails
inspection(_, []):- !.
inspection(F, [X|Xs]):- call(F, X), inspection(F, Xs).

zip([A], [B], [(A, B)]):- !.
zip([A|L], [B|R], [(A, B)|S]):- zip(L, R, S), !.

% F :: a -> a -> a
zip_with(_,  [],  [],  []):- !.
zip_with(F, [A], [B], [X]):- call(F, A, B, X), !.
zip_with(F, [A|L], [B|R], [C|S]):- call(F, A, B, C), zip_with(F, L, R, S), !.

list_concat([], L, L).
list_concat([A|L], R, [A|C]):- list_concat(L, R, C).

% F :: a -> b -> a
% foldl F x (y:ys) = foldl F (F x y) ys
foldl(_, X, [], X):- !.
foldl(F, X, [Y|L], R):- call(F, X, Y, S), foldl(F, S, L, R).

% F :: a -> b -> b
% foldr f x (y:ys) = f y (foldr f x ys)
foldr(_, X, [], X):- !.
foldr(F, X, [Y|L], R):- foldr(F, X, L, S), call(F, Y, S, R).

% creates a list with K elements equal to S
padded_list(0, _, []):- !.
padded_list(K, S, [S|R]):- K1 is K - 1, padded_list(K1, S, R), !.

% adds at the beggining/ending of the list L K elements equal to S
padded_list_begin(L, K, S, R):- padded_list(K, S, P), list_concat(P, L, R).
padded_list_end(L, K, S, R):- padded_list(K, S, P), list_concat(L, P, R).

% cartesian_product(A, B, C): C is the cartesian product of A and B.
% C = A x B
cartesian_product([], _, []):- !.
cartesian_product(_, [], []):- !.
cartesian_product([X], [Y|R], [[X,Y]|S]):-
	cartesian_product([X], R, S), !.
cartesian_product([X|L], R, S):-
	cartesian_product([X], R, S1), cartesian_product(L, R, S2),
	list_concat(S1, S2, S), !.

% cartesian_product_by(F, A, B, C): C is the result of applying the function
% F to every element of (A x B).
% C = map(F, A x B)
cartesian_product_by(_, [], _, []):- !.
cartesian_product_by(_, _, [], []):- !.
cartesian_product_by(F, [X], [Y|R], [E|S]):-
	call(F, X, Y, E), cartesian_product_by(F, [X], R, S), !.
cartesian_product_by(F, [X|L], R, S):-
	cartesian_product_by(F, [X], R, S1), cartesian_product_by(F, L, R, S2),
	list_concat(S1, S2, S), !.

% SORTING ALGORITHMS
% F(X,Y) is true when X < Y

% inserts element X in list L, according to function F
insert_by(_, X, [], [X]):- !.
insert_by(F, X, [Y|L], [Y|R]):-
	not(call(F, X, Y)),        % check X >= Y
	insert_by(F, X, L, R), !.  % if so, insert X in L
insert_by(_, X, [Y|L], [X,Y|L]).   % X < Y

% insertion sort (as always)
isort_by(_, [], []):- !.
isort_by(_, [X], [X]):- !.
isort_by(F, [X|L], R):- isort_by(F, L, S), !, insert_by(F, X, S, R).

% pairwise insertion sort
% In short, given two lists A,B of the same length, and two elements
% a,b, inserts a into A. If position of 'a' in 'A' is 'p', then position
% of 'b' in 'B' is also p. Note that B may not be sorted at the end.

% inserts two elements, each to a different list, according function F,
% guided by the first list
pinsert_by(_, X,Y,      [],     [],       [X],      [Y]):- !.
pinsert_by(F, X,Y, [Xx|Xs],[Yy|Ys],   [Xx|Xr],  [Yy|Yr]):-
	not(call(F,X,Xx)),   % check X >= Xx
	pinsert_by(F, X,Y, Xs,Ys, Xr,Yr), !.
pinsert_by(_, X,Y, [Xx|Xs],[Yy|Ys], [X,Xx|Xs],[Y,Yy|Ys]).

% insertion sort of two lists using function F, guided by first list
pisort_by(_,     [],    [],  [], []):- !.
pisort_by(_,    [X],   [Y], [X],[Y]):- !.
pisort_by(F, [X|Xs],[Y|Ys],  Rx, Ry):-
	pisort_by(F, Xs,Ys, Sx,Sy),
	pinsert_by(F, X,Y, Sx,Sy, Rx,Ry).

% merges two same-sized sorted lists into one, according to function F
merge_by(_,     [],     [],       []):- !.
merge_by(_,     [],      L,        L):- !.
merge_by(_,      L,     [],        L):- !.
merge_by(F,    [X],    [Y],    [X,Y]):- call(F, X, Y), !.
merge_by(_,    [X],    [Y],    [Y,X]):- !.
merge_by(F, [X|Xs], [Y|Ys], [X,Y|Ms]):- call(F, X, Y), !, merge_by(F, Xs, Ys, Ms).
merge_by(F, [X|Xs], [Y|Ys], [Y,X|Ms]):- merge_by(F, Xs, Ys, Ms).

% merges four same-sized sorted lists into two, pairwise, according to
% function F, in the same fashion as pisort_by sorts two lists
pmerge_by(_, [],[], [],[], [],[]):- !.
pmerge_by(_, [],[],  V, E,  V, E):- !.
pmerge_by(_,  V, E, [],[],  V, E):- !.
pmerge_by(F, [X1|Xs],[Y1|Ys], [X2|Xr],[Y2|Yr], [X1,X2|Xz],[Y1,Y2|Yz]):-
	call(F, X1, X2), pmerge_by(F, Xs,Ys, Xr,Yr, Xz,Yz), !.
pmerge_by(F, [X1|Xs],[Y1|Ys], [X2|Xr],[Y2|Yr], [X2,X1|Xz],[Y2,Y1|Yz]):-
	pmerge_by(F, Xs,Ys, Xr,Yr, Xz,Yz).

% macros
insert(X, L, R):- insert_by(@<, X, L, R).
pinsert(X,Y, Xs,Ys, SX,SY):- pinsert_by(@<, X,Y, Xs,Ys, SX,SY).
isort(L, R):- isort_by(@<, L, R).
pisort(X,Y, SX,SY):- pisort_by(@<, X,Y, SX,SY).
merge(X,Y, M):- merge_by(@<, X,Y, M).
pmerge(X,Y, SX,SY, RX,RY):- pmerge_by(@<, X,Y, SX,SY, RX,RY).

% COUNTING FUNCTIONS

how_many_([X], [[X, 1]]):- !.
how_many_([X|L], [[X, C] | RR]):-
	how_many_(L, R), first(R, [X, N], RR), !, C is N + 1.
how_many_([X|L], [[X, 1] | R]):-
	how_many_(L, R).

how_many(L, R):- isort(L, S), how_many_(S, R).

% MATHEMATICAL OPERATIONS

/*
X = [x1, x2, ..., xn]
Y = [y1, y2, ..., yn]
R = x1*y1 + x2*y2 + ... + xn*yn
*/
dot_prod([X], [Y], P):- P is X*Y, !.
dot_prod([X|Xs], [Y|Ys], R):- P is X*Y, dot_prod(Xs, Ys, Q), R is P + Q, !.

/*
  | m n o p
x | 0 B C D
--+-------------
  | B C D P

B = x*(m + 0)
C = x*(n + B)
D = x*(o + C)
P = x*(p + D)

L = [B,C,D]
*/
ladder_prod(X, A, [Y], [], P):-
	P is X*(Y + A), !.
ladder_prod(X, A, [Y|Ys], [R|L], Q):-
	R is Y + A, P is X*R, ladder_prod(X, P, Ys, L, Q), !.

