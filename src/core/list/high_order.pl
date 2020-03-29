/*********************************************************************
 * Sycalc
 * Copyright (C) 2018,2019,2020  Lluís Alemany Puig
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
	@descr This file contains a number of high-order predicates for
	list manipulation, such as map, foldl, foldr, zip, ...
*/

/**
	@form map(Function, List, NewList)
	@descr @NewList is the result of applying Function to every element
	of @List.
	
	In Haskell notation:
	\bverbatim
	map :: (a -> b) -> [a] -> [b]
	\everbatim
*/
map(_, [], []):- !.
map(F, [X|L], [E|R]):- call(F, X, E), map(F, L, R), !.

/**
	@form inspection(Function, List)
	@descr The inspection of @List with Function is the verification
	that applying Function to all elements in @List does not fail. To
	check this, the function is applied to every element of @List.
*/
inspection(_, []):- !.
inspection(F, [X|Xs]):- call(F, X), inspection(F, Xs).

/**
	@form zip(List1, List2, NewList)
	@descr @NewList is the result of an element-wise pairing of the
	elements in @List1 and @List2. Each of NewList is a pair, where
	the left element is an element from @List1 and the right element
	is an element from @List2.
	
	In Haskell notation: 
	\bverbatim
	zip :: [a] -> [b] -> [(a, b)]
	\everbatim
	@constrs @List1 and @List2 must have the same length.
*/
zip([], [], []).
zip([A|L], [B|R], [(A, B)|S]):- zip(L, R, S).

/**
	@form zip(Function, List1, List2, NewList)
	@descr @NewList is the result of applying Function to the
	i-th element of both @List1 and @List2.
	
	In Haskell notation: 
	\bverbatim
	zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
	\everbatim
	@constrs @List1 and @List2 must have the same length.
*/
zip_with(_,  [],  [],  []):- !.
zip_with(F, [A], [B], [X]):- call(F, A, B, X), !.
zip_with(F, [A|L], [B|R], [C|S]):- call(F, A, B, C), zip_with(F, L, R, S).

/**
	@form list_concat(List1, List2, NewList)
	@descr @NewList is a list with all the elements in @List1 followed
	by all the elements in @List2.
*/
list_concat([], L, L).
list_concat([A|L], R, [A|C]):- list_concat(L, R, C).

/**
	@form foldl(Function, Value, List, Result)
	@descr foldl as usually defined.
	
	In Haskell notation:
	\bverbatim
	foldl :: (b -> a -> b) -> b -> a -> b
	\everbatim
*/
foldl(_, X, [], X):- !.
foldl(F, X, [Y|L], R):- call(F, X, Y, S), foldl(F, S, L, R).

/**
	@form foldr(Function, Value, List, Result)
	@descr foldr as usually defined.
	
	In Haskell notation:
	\bverbatim
	foldr :: (a -> b -> b) -> b -> t a -> b
	\everbatim
*/
foldr(_, X, [], X):- !.
foldr(F, X, [Y|L], R):- foldr(F, X, L, S), call(F, Y, S, R).

/**
	@form make_list(Times, Value, NewList)
	@constrs @Times must be a positive (>= 0) integer.
	@descr @NewList is a list containing Value as many times as @Times.
*/
make_list(0, _, []):- !.
make_list(K, S, [S|R]):- K1 is K - 1, make_list(K1, S, R).

/**
	@form pad_begin(Times, List, Value, NewList)
	@constrs @Times must be a positive (>= 0) integer.
	@descr @NewList is the concatenation of a list of length @Times
	with all elements equal to @Value and @List.
*/
pad_begin(K, L, S, R):- make_list(K, S, B), list_concat(B, L, R).

/**
	@form pad_end(Times, List, Value, NewList)
	@constrs @Times must be a positive (>= 0) integer.
	@descr @NewList is the concatenation of @List and a list of length
	@Times with all elements equal to @Value.
*/
pad_end(K, L, S, R):- make_list(K, S, E), list_concat(L, E, R).

/**
	@form cartesian_product(List1, List2, CP)
	@descr @CP is the cartesian product of @List1 and @List2. Every element
	of @CP is a list of two elements.
*/
cartesian_product([], _, []):- !.
cartesian_product(_, [], []):- !.
cartesian_product([X], [Y|R], [[X,Y]|S]):- cartesian_product([X], R, S), !.
cartesian_product([X|L], R, S):-
	cartesian_product([X], R, S1), cartesian_product(L, R, S2),
	list_concat(S1, S2, S), !.

/**
	@form cartesian_product_by(Function, List1, List2, CP)
	@descr @CP is the result of applying Function to every element
	of the cartesian product of @List1 and @List2.
	
	This predicate performs a series of operations similar to doing:
	
	\bverbatim
	cartesian_product_by(F, List1,List2, R):- 
		cartesian_product(List1,List2, C), map(F,C, R)
	\everbatim
	
	However, this implementation should be faster because it does
	a single pass on the elements of the cartesian product.
*/
cartesian_product_by(_, [], _, []):- !.
cartesian_product_by(_, _, [], []):- !.
cartesian_product_by(F, [X], [Y|R], [E|S]):-
	call(F, X, Y, E), cartesian_product_by(F, [X], R, S), !.
cartesian_product_by(F, [X|L], R, S):-
	cartesian_product_by(F, [X], R, S1), cartesian_product_by(F, L, R, S2),
	list_concat(S1, S2, S), !.
