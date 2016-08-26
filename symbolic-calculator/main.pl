:-include(lists).
:-include(monomials).
:-include(polynomials).

apply_func(M):- write(M), write(': '), reduced_monomial(M, R), write(R), nl.

main:- 	apply_func(0*x^0),
		apply_func(0*x^1),
		apply_func(0*x^5),
		
		apply_func(1*x^0),
		apply_func(1*x^1),
		apply_func(1*x^5),
		
		apply_func(3*x^0),
		apply_func(3*x^1),
		apply_func(3*x^6),
		
		apply_func((0/1)*x^(0/1)),
		apply_func((0/8)*x^(4/4)),
		apply_func((0/3)*x^(20/4)),
		
		apply_func((7/7)*x^(0/9)),
		apply_func((6/6)*x^(2/2)),
		apply_func((1/1)*x^(5/1)),
		
		apply_func((27/9)*x^(0/1)),
		apply_func((18/6)*x^(8/8)),
		apply_func((15/5)*x^(36/6))
		.