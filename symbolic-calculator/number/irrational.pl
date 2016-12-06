:- ensure_loaded(rational).

% IRRATIONALS

irrational(A):- not(rational(A)), number(A).