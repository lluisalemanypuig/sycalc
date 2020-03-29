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

:-ensure_loaded(natural).
:-ensure_loaded(integer).
:-ensure_loaded(fraction).
:-ensure_loaded(rational).
:-ensure_loaded(irrational).
:-ensure_loaded(real).

% ARITHMETIC EVALUATION

/***
	@descr This file contains all predicates needed to evaluate arithmetic
	expressions between real numbers, where the result of operating
	two rational numbers is another rational number.

	For example, the
	result of 2/3 + 4/5 is not given as 1.46667, but as 22/15.
*/

/**
	@form eval_sum(A,B, S)
	@descr @S is the summation of @A and @B.
*/
eval_sum(A, B, C):- fraction(A), fraction(B), frac_sum(A, B, R), red_frac(R, C), !.
eval_sum(A, B, C):- fraction(A), frac_sum(A, B/1, R), red_frac(R, C), !.
eval_sum(A, B, C):- fraction(B), frac_sum(A/1, B, R), red_frac(R, C), !.
eval_sum(A, B, C):- C is A + B.

/**
	@form eval_sub(A,B, S)
	@descr @S is the substraction of @A from @B.
*/
eval_sub(A, B, C):- fraction(A), fraction(B), frac_sub(A, B, R), red_frac(R, C), !.
eval_sub(A, B, C):- fraction(A), frac_sub(A, B/1, R), red_frac(R, C), !.
eval_sub(A, B, C):- fraction(B), frac_sub(A/1, B, R), red_frac(R, C), !.
eval_sub(A, B, C):- C is A - B.

/**
	@form eval_prod(A,B, P)
	@descr @P is the product of @A and @B.
*/
eval_prod(A, B, C):- fraction(A), fraction(B), frac_prod(A, B, R), red_frac(R, C), !.
eval_prod(A, B, C):- fraction(A), frac_prod(A, B/1, R), red_frac(R, C), !.
eval_prod(A, B, C):- fraction(B), frac_prod(A/1, B, R), red_frac(R, C), !.
eval_prod(A, B, C):- C is A*B.

/**
	@form eval_div(A,B, D)
	@descr @D is the result of dividing @A and @B.
*/
eval_div(A, B, C):- fraction(A), fraction(B), frac_div(A, B, R), red_frac(R, C), !.
eval_div(A, B, C):- fraction(A), frac_div(A, B/1, R), red_frac(R, C), !.
eval_div(A, B, C):- fraction(B), frac_div(A/1, B, R), red_frac(R, C), !.
eval_div(A, B, C):- multiple(A, B), C is A/B.
eval_div(A, B, A/B).

/**
	@form eval_pow(A,B, P)
	@descr @P is the result of raising @A to the power @B.
	@constrs
		@param B A natural value.
*/
eval_pow(A, B, C):- fraction(A), frac_pow(A, B, R), red_frac(R, C), !.
eval_pow(A, B, C):- C is A^B.

/**
	@form eval_pow(A, N)
	@descr @N is the result of multiplying by (-1) the value @A.
*/
eval_neg(A, B):- fraction(A), neg_frac(A, B), !.
eval_neg(A, B):- B is -A, !.

% ARITHMETIC EXPRESSIONS

/**
	@form(Expr)
	@descr Definition of arithmetic expression: this predicate fails on
	all values of @Expr that are not expressions:
	\bverbatim
		A + B
		A - B
		A*B
		A/B
		A^B
	\everbatim
*/
expr(_ + _):- !.
expr(_ - _):- !.
expr(_*_):- !.
expr(_/_):- !.
expr(_^_):- !.

/**
	@form arith_expr_eval(Expr, Result)
	@constraint @Expr is an arithmetic expression. That is, it is either
	a real value, or an arithmetic expression of the form
	\bverbatim
		A + B
		A - B
		A*B
		A/B
		A^B
		-A
	\everbatim
	where @A and @B are arithmetic expressions.
*/
arith_expr_eval(A + B, C):- arith_expr_eval(A, AA), arith_expr_eval(B, BB), eval_sum(AA, BB, C), !.
arith_expr_eval(A - B, C):- arith_expr_eval(A, AA), arith_expr_eval(B, BB), eval_sub(AA, BB, C), !.
arith_expr_eval(A*B, C):- arith_expr_eval(A, AA), arith_expr_eval(B, BB), eval_prod(AA, BB, C), !.
arith_expr_eval(A/B, C):- arith_expr_eval(A, AA), arith_expr_eval(B, BB), red_frac(AA/BB, C), !.
arith_expr_eval(A^B, C):- arith_expr_eval(A, AA), arith_expr_eval(B, BB), eval_pow(AA, BB, C), !.
arith_expr_eval(-A, C):- arith_expr_eval(A, AA), eval_neg(AA, C), !.
arith_expr_eval(A, A):- real(A).

% Computes the factorial of a natural number N,
factorial_(0, 1):- !.
factorial_(N, F):- N1 is N - 1, factorial_(N1, F1), F is N*F1.
/**
	@form factorial(Expr, F)
	@constrs Expr is an arithmetic expression.
	@descr @F is the factorial of the result of eavluating the arithmetic
	expression @Expr.
*/
factorial(E, F):- expr(E), arith_expr_eval(E, N), natural(N), factorial_(N, F), !.
factorial(N, F):- natural(N), factorial_(N, F).
