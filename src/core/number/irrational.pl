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

:- ensure_loaded(rational).

% IRRATIONALS

/***
	@descr This file contains the definition of irrational numbers.
*/

/**
	@form irrational(A)
	@descr @A is an irrational number if it is not a rational number.
*/
irrational(A):- not(rational(A)), number(A).
