/*********************************************************************
 * Sycalc
 * Copyright (C) 2018  Lluís Alemany Puig
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Contact: Lluís Alemany Puig (lluis.alemany.puig@gmail.com)
 * 
 ********************************************************************/

/***
	@descr This file contains a number of simple list-related simple
	predicates. Finding the minimum and maximum values in a list, sorting
	a list, dropping certain elements from a list, ...
*/

/**
	@form min(List, Min)
	@descr @Min is the smallest value, according to '@>', in @List.
	@constrs
		@param List Must have at least one element.
*/
min([X], X):- !.
min([X|L], M):- min(L, N), X @> N, !, M is N.
min([X|_], X).

/**
	@form max(List, Max)
	@descr @Max is the smallest value, according to '@<', in @List.
	@constrs
		@param List Must have at least one element.
*/
max([X], X):- !.
max([X|L], M):- max(L, N), X @< N, !, M is N.
max([X|_], X).

/**
	@form first(List, First, Rest)
	@descr @First is the head of @List and @Rest are the other elements
	of @List.
	@constrs
		@param List Must have at least one element.
*/
first([X], X, []):- !.
first([X|L], X, L).

/**
	@form last(List, Rest, Last)
	@descr @Last is the last element of @List. @Rest are the elements
	from the first to the second to last.
	@constrs
		@param List Must have at least one element.
*/
last([X], [], X):- !.
last([X|R], [X|K], L):- last(R, K, L).

/**
	@form replace_first(List, Value, NewList)
	@descr @NewList is @List but its first value is now @Value. @NewList
	has the same length as @List.
	@constrs
		@param List Must have at least one element.
*/
replace_first([_], Y, [Y]):- !.
replace_first([_|L], Y, [Y|L]).

/**
	@form replace_last(List, Value, NewList)
	@descr @NewList is @List but the last value is now @Value. @NewList
	has the same length as @List.
	@constrs
		@param List Must have at least one element.
*/
replace_last([_], Y, [Y]):- !.
replace_last([X|L], Y, [X|R]):- replace_last(L, Y, R).

/**
	@form replace(Value,With, List, NewList)
	@descr This predicete replaces all elements equal to @Value with
	value @With in @List. @NewList is the result of this substitution.
*/
replace(_, _,     [],     []):- !.
replace(V, I,    [V],    [I]):- !.
replace(_, _,    [X],    [X]):- !.
replace(V, I, [V|Xs], [I|Ds]):- replace(V, I, Xs, Ds), !.
replace(V, I, [X|Xs], [X|Ds]):- replace(V, I, Xs, Ds).

/**
	@form drop(Value, List, NewList)
	@descr @NewList contains all elements from @List except for those
	equal to @Value, according to '\='.
*/
drop(_,    [],      []):- !.
drop(O,    [O],     []):- !.
drop(_,    [X],    [X]):- !.
drop(O, [O|Xs],      D):- drop(O, Xs, D), !.
drop(O, [X|Xs], [X|Ds]):- drop(O, Xs, Ds).

/**
	@form pdrop(Value, List1,List2, NewList1,NewList2)
	@descr @NewList1 contains all elements from @List1 except for those
	equal to @Value, according to '\='. If i-th element of @List1 is
	dropped then the i-th element of @List2 is also dropped and
	@NewList2 will not contain it.
	@constrs @List1 and @List2 must have the same length.
*/
pdrop(_,         [],[],           [],[]):- !.
pdrop(X,       [X],[_],           [],[]):- !.
pdrop(_,       [X],[Q],         [X],[Q]):- !.
pdrop(X, [X|Xs],[_|Qs],         Dxs,Dqs):- pdrop(X, Xs,Qs, Dxs,Dqs), !.
pdrop(X, [Y|Xs],[Q|Qs], [Y|Dxs],[Q|Dqs]):- pdrop(X, Xs,Qs, Dxs,Dqs).

/**
	@form split(Value,List, Left,Right)
	@descr Splits @List into two groups, @Left and @Right. @Left contains
	all elements in @List equal to @Value. @Right contains all the other
	elements in @List. The sum of lengths of @Left and @Right equals the
	length of @List.
	@constrs @List must have at least one element.
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
	@descr Splits lists @List1 and @List2 each into two groups. The
	splitting is done similarly as in predicate ?split/4. However, here
	the splitting is guided by @List1, that is:
	\blist
		\item if i-th element of @List1 is equal to @Value then @L1 will
		contain i-th element of @List1 and @L2 will contain i-th element
		of @List2.
		\item if i-th element of @List1 is not equal to @Value then @R1 will
		contain i-th element of @List1 and @R2 will contain i-th element
		of @List2.
	++>
	@constrs @List1 and @List2 must be non-empty and of equal length.
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
	@descr @Counting is a list containing lists of two elements, the
	first of which is an element of @List and the last is the number of
	occurrences of that element in @List.
	@constrs @List cannot be empty.
*/
how_many(L, R):- isort(L, S), how_many_(S, R).

% MATHEMATICAL OPERATIONS

/**
	@form dot_prod(List1, List2, P)
	@descr @P is the result of applying the dot product to the lists
	@List1 and @List2 as if they were vectors.
	\bverbatim
	List1 = [x1, x2, ..., xn]
	List2 = [y1, y2, ..., yn]
	P = x1*y1 + x2*y2 + ... + xn*yn
	\everbatim
	@constrs @List1 and @List2 cannot be empty, both must contain
	elements to which the operators '*' and '+' can be applied, and
	must have the same length.
*/
dot_prod([X], [Y], P):- P is X*Y, !.
dot_prod([X|Xs], [Y|Ys], R):- P is X*Y, dot_prod(Xs, Ys, Q), R is P + Q, !.

/**
	@form ladder_prod(Divisor, StartValue, Coefficients, NewCoefficients, LastTerm)
	@descr One should interpret this predicate as some sort of Ruffini's
	rule generalisation.
	Therefore, if @Coefficients has the elements [m,n,o,p], @Divisor has
	the value 'x' and @StartValue is 'A' then the ladder product is
	defined as follows:
	\bverbatim
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
	\everbatim
	For the case of Ruffini's rule:
	\blist
	\item @Coefficients must be a list of integer values with a '1' in the
	first element.
	\item @StartValue (A) has to be 0
	\item @Divisor must divide the last element of @Coefficients
	\item @NewCoefficients are the integer values representing the coefficients
	of a new polynomial.
	\item @LastTerm must be equal to 0 for @Divisor to be a root of the
	polynomial represented by @Coefficients.
	++>
	@constrs @Coefficients cannot be empty and must contain
	elements to which the operators '*' and '+' can be applied.
*/
ladder_prod(X, A, [Y], [], P):- P is X*(Y + A), !.
ladder_prod(X, A, [Y|Ys], [R|L], Q):-
	R is Y + A, P is X*R, ladder_prod(X, P, Ys, L, Q), !.

