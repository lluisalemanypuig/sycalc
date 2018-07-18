/***
	@descr This file contains a number of simple list-related simple
	predicates. Finding the minimum and maximum values in a list, sorting
	a list, dropping certain elements from a list, ...
*/

/**
	@form min(List, Min)
	@constrs List must have at least one element (what is the minimum
	value of a list with no values?).
	@descr Min is the smallest value, according to '@>' in List
*/
min([X], X):- !.
min([X|L], M):- min(L, N), X @> N, !, M is N.
min([X|_], X).

/**
	@form max(List, Max)
	@constrs List must have at least one element (what is the maximum
	value of a list with no values?).
	@descr Max is the smallest value, according to '@<' in List
*/
max([X], X):- !.
max([X|L], M):- max(L, N), X @< N, !, M is N.
max([X|_], X).

/**
	@form first(List, First, Rest)
	@constrs List cannot be empty.
	@descr First is the head of List and Rest are the other elements of
	List.
*/
first([X], X, []):- !.
first([X|L], X, L).

/**
	@form last(List, Rest, Last)
	@constrs List cannot be empty.
	@descr Last is the last element of List. Rest are the elements from
	the first to the second to last.
*/
last([X], [], X):- !.
last([X|R], [X|K], L):- last(R, K, L).

/**
	@form replace_first(List, Value, NewList)
	@constrs List cannot be empty.
	@descr NewList is List but the first value is now Value. NewList
	has the same length as List.
*/
replace_first([_], Y, [Y]):- !.
replace_first([_|L], Y, [Y|L]).

/**
	@form replace_last(List, Value, NewList)
	@constrs List cannot be empty.
	@descr NewList is List but the last value is now Value. NewList
	has the same length as List.
*/
replace_last([_], Y, [Y]):- !.
replace_last([X|L], Y, [X|R]):- replace_last(L, Y, R).

/**
	@form replace(Value,With, List, NewList)
	@descr This predicete replaces all elements equal to Value with
	value With in List. NewList is the result of this substitution.
*/
replace(_, _,     [],     []):- !.
replace(V, I,    [V],    [I]):- !.
replace(_, _,    [X],    [X]):- !.
replace(V, I, [V|Xs], [I|Ds]):- replace(V, I, Xs, Ds), !.
replace(V, I, [X|Xs], [X|Ds]):- replace(V, I, Xs, Ds).

/**
	@form drop(Value, List, NewList)
	@descr NewList contains all elements from List except for those
	equal to Value, according to \=.
*/
drop(_,    [],      []):- !.
drop(O,    [O],     []):- !.
drop(_,    [X],    [X]):- !.
drop(O, [O|Xs],      D):- drop(O, Xs, D), !.
drop(O, [X|Xs], [X|Ds]):- drop(O, Xs, Ds).

/**
	@form pdrop(Value, List1,List2, NewList1,NewList2)
	@constrs List1 and List2 must have the same length.
	@descr NewList1 contains all elements from List1 except for those
	equal to Value, according to \=. If i-th element of List1 is
	dropped then the i-th element of List2 is also dropped and NewList2
	will not contain it.
*/
pdrop(_,         [],[],           [],[]):- !.
pdrop(X,       [X],[_],           [],[]):- !.
pdrop(_,       [X],[Q],         [X],[Q]):- !.
pdrop(X, [X|Xs],[_|Qs],         Dxs,Dqs):- pdrop(X, Xs,Qs, Dxs,Dqs), !.
pdrop(X, [Y|Xs],[Q|Qs], [Y|Dxs],[Q|Dqs]):- pdrop(X, Xs,Qs, Dxs,Dqs).

/**
	@form split(Value,List, Left,Right)
	@constrs List can not be empty.
	@descr Splits List into two groups, Left and Right. Left contains
	all elements in List equal to Value. Right contains all the other
	elements in List. The sum of lengths of Left and Right equals the
	length of List.
*/
split(X,    [X],    [X],[]):- !.
split(X,    [X],    [],[X]):- !, false.
split(_,    [Y],    [],[Y]):- !.
split(X, [X|Xx], [X|Xs],Lr):- split(X, Xx, Xs,Lr), !.
split(X,  [X|_],   _,[X|_]):- !, false.
split(X, [Y|Yy], Xs,[Y|Lr]):- split(X, Yy, Xs,Lr).

/*
OLD SPLIT
split(X,    [Y],    [Y],[]):- X == Y, !.
split(X,    [Y],    [],[Y]):- X \= Y, !.
split(X, [Y|Xx], [Y|Xs],Lr):- X == Y, split(X, Xx, Xs,Lr), !.
split(X, [Y|Yy], Xs,[Y|Lr]):- X \= Y, split(X, Yy, Xs,Lr).
*/

/**
	@form split(Value, List1,List2, L1,L2, R1,R2)
	@constrs List1 and List2 must be non-empty and of equal length.
	@descr Splits lists List1 and List2 each into two groups. The splitting
	is done similarly as in predicate 'split'. However, here the splitting
	is guided by List1, that is:
		* if i-th element of List1 is equal to Value then L1 will contain
		i-th element of List1 and L2 will contain i-th element of List2.
		* if i-th element of List1 is not equal to Value then R1 will
		contain i-th element of List1 and R2 will contain i-th element
		of List2.
*/
psplit(X,       [X],[Q],         [X],[Q],           [],[]):- !.
psplit(X,       [X],[Q],           [],[],         [X],[Q]):- !, false.
psplit(_,       [Y],[Q],           [],[],         [Y],[Q]):- !.

psplit(X, [X|L1],[Q|L2], [X|Ls1],[Q|Ls2],         Rs1,Rs2):- psplit(X, L1,L2, Ls1,Ls2, Rs1,Rs2), !.
psplit(X,       [X|_],_,             _,_,         [X|_],_):- !, false.
psplit(X, [Y|L1],[Q|L2],         Ls1,Ls2, [Y|Rs1],[Q|Rs2]):- psplit(X, L1,L2, Ls1,Ls2, Rs1,Rs2), !.

/*
OLD PSPLIT

F(X,       [X],[Q],         [X],[Q],           [],[]):- !.
F(X,       [Y],[Q],           [],[],         [Y],[Q]):- X \= Y, !.
F(X, [X|L1],[Q|L2], [X|Ls1],[Q|Ls2],         Rs1,Rs2):- F(X, L1,L2, Ls1,Ls2, Rs1,Rs2), !.
F(X, [Y|L1],[Q|L2],         Ls1,Ls2, [Y|Rs1],[Q|Rs2]):- X \= Y, F(X, L1,L2, Ls1,Ls2, Rs1,Rs2).
*/


% COUNTING FUNCTIONS

how_many_([X], [[X, 1]]):- !.
how_many_([X|L], [[X, C] | RR]):- how_many_(L, R), first(R, [X, N], RR), !, C is N + 1.
how_many_([X|L], [[X, 1] | R]):- how_many_(L, R).

/**
	@form how_many(List, Counting)
	@constrs List cannot be empty.
	@descr Counting is a list containing lists of two elements, the
	first of which is an element of List and the last is the number of
	occurrences of that element in List.
*/
how_many(L, R):- isort(L, S), how_many_(S, R).

% MATHEMATICAL OPERATIONS

/**
	@form dot_prod(List1, List2, P)
	@constrs List1 and List2 cannot be empty, both must contain
	elements to which the operators '*' and '+' can be applied, and
	must have the same length.
	@descr P is the result of applying the dot product to the lists
	List1 and List2 as if they were vectors.
	List1 = [x1, x2, ..., xn]
	List2 = [y1, y2, ..., yn]
	P = x1*y1 + x2*y2 + ... + xn*yn
*/
dot_prod([X], [Y], P):- P is X*Y, !.
dot_prod([X|Xs], [Y|Ys], R):- P is X*Y, dot_prod(Xs, Ys, Q), R is P + Q, !.

/**
	@form ladder_prod(Divisor, StartValue, Coefficients, NewCoefficients, LastTerm)
	@constrs Coefficients cannot be empty and must contain
	elements to which the operators '*' and '+' can be applied.
	@descr One should interpret this predicate as some sort of Ruffini's
	rule generalisation.
	Therefore, if Coefficients has the elements [m,n,o,p], Divisor has
	the value 'x' and StartValue is 'A' then the ladder product is
	defined as follows:

	  | m n o p
	x | A B C D
	--+-------------
	  | T U V W
	T = m + A
	B = x*T = x*(m + A)
	U = n + B
	C = x*U = x*(n + B)
	V = o + C
	D = x*V = x*(o + C)
	LastTerm = W = p + D
	NewCoefficients = [T,U,V]

	For the case of Ruffini's rule:
	-> Coefficients must be a list of integer values with a '1' in the
	first element.
	-> StartValue (A) has to be 0
	-> Divisor must divide the last element of Coefficients
	-> NewCoefficients are the integer values representing the coefficients
	of a new polynomial.
	-> LastTerm must be equal to 0 for Divisor to be a root of the
	polynomial represented by Coefficients.
*/
ladder_prod(X, A, [Y], [], P):- P is X*(Y + A), !.
ladder_prod(X, A, [Y|Ys], [R|L], Q):-
	R is Y + A, P is X*R, ladder_prod(X, P, Ys, L, Q), !.

