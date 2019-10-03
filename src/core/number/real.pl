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

:-ensure_loaded(rational).
:-ensure_loaded(irrational).

% REALS

/***
	@descr This file contains the definition of a real number (either
	a rational or an irrational number) and the definition of absolute
	value of such a number.
*/

/**
	@form real(A)
	@descr This predicate failes if @A is neither a rational or an
	irrational value.
*/
real(A):- rational(A), !.
real(A):- irrational(A).

% Compute the absolute value of a real number
/**
	@form abs_real(X, A)
	@descr @A is the aboslute value of @X.
	@constrs @X is a real number.
*/
abs_real(X, AX):- fraction(X), X < 0, neg_frac(X, AX), !.
abs_real(X, X):- fraction(X), X > 0, !.
abs_real(X, AX):- abs(X, AX).

