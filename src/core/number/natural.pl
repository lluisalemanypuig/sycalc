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
	@descr This file contains the basic definitions for what a natural
	number is, two constants (zero and one), 
*/

/**
	@form zero(Z)
	@descr Predicate fails if @Z is not 0.
*/
zero(0).
/**
	@form one(O)
	@descr Predicate fails if @O is not 1.
*/
one(1).

/**
	@form natural(N)
	@descr Predicates fails on any non-integer value, or negative
	integer value.
*/
natural(N):- integer(N), N >= 0.

/**
	@form next_natural(N)
	@descr N is a natural. Example of usage:
	\bverbatim
		next_natural(N), write(N), nl, fail.
	\everbatim
*/
next_natural(0).
next_natural(N):- next_natural(M), N is M + 1.

