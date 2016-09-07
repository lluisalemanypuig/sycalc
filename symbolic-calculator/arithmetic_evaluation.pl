:-ensure_loaded(numerical_algorithms).
:-ensure_loaded(numbers).

% ARITHMETIC EXPRESSIONS' EVALUATION

% C is A + B
eval_sum(A, B, C):- fraction(A), fraction(B), frac_sum(A, B, R), red_frac(R, C), !.
eval_sum(A, B, C):- fraction(A), frac_sum(A, B/1, R), red_frac(R, C), !.
eval_sum(A, B, C):- fraction(B), frac_sum(A/1, B, R), red_frac(R, C), !.
eval_sum(A, B, C):- C is A + B.

% C is A - B
eval_sub(A, B, C):- fraction(A), fraction(B), frac_sub(A, B, R), red_frac(R, C), !.
eval_sub(A, B, C):- fraction(A), frac_sub(A, B/1, R), red_frac(R, C), !.
eval_sub(A, B, C):- fraction(B), frac_sub(A/1, B, R), red_frac(R, C), !.
eval_sub(A, B, C):- C is A - B.

% C is A*B
eval_prod(A, B, C):- fraction(A), fraction(B), frac_prod(A, B, R), red_frac(R, C), !.
eval_prod(A, B, C):- fraction(A), frac_prod(A, B/1, R), red_frac(R, C), !.
eval_prod(A, B, C):- fraction(B), frac_prod(A/1, B, R), red_frac(R, C), !.
eval_prod(A, B, C):- C is A*B.

% C is A/B
eval_div(A, B, C):- fraction(A), fraction(B), frac_div(A, B, R), red_frac(R, C), !.
eval_div(A, B, C):- fraction(A), frac_div(A, B/1, R), red_frac(R, C), !.
eval_div(A, B, C):- fraction(B), frac_div(A/1, B, R), red_frac(R, C), !.
eval_div(A, B, C):- multiple(A, B), C is A/B.
eval_div(A, B, A/B).

% C is A^B
eval_pow(A, B, C):- fraction(A), fraction(B), frac_pow(A, B, R), red_frac(R, C), !.
eval_pow(A, B, C):- fraction(A), frac_pow(A, B/1, R), red_frac(R, C), !.
eval_pow(A, B, C):- fraction(B), frac_pow(A/1, B, R), red_frac(R, C), !.
eval_pow(A, B, C):- C is A^B.

% ARITHMETIC EVALUATION

% An arithmetic expression is a sum, sub, prod, div or pow of real numbers
arithmetic_eval(A + B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_sum(AA, BB, C), !.
arithmetic_eval(A - B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_sub(AA, BB, C), !.
arithmetic_eval(A*B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_prod(AA, BB, C), !.
arithmetic_eval(A/B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), red_frac(AA/BB, C), !.
arithmetic_eval(A^B, C):- arithmetic_eval_pow(A^B, C), !.
arithmetic_eval(A^B, C):- arithmetic_eval(A, AA), arithmetic_eval(B, BB), eval_pow(AA, BB, C), !.
arithmetic_eval(A, A):- real(A).

arithmetic_eval_pow(A^B^C, R):- arithmetic_eval(A^B, R1), arithmetic_eval(R1^C, R).

% EXPRESSIONS

expr(_ + _).
expr(_ - _).
expr(_*_).
expr(_/_).
expr(_^_).
