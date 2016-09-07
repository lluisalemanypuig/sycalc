/*
-----------------------------
DEBUG - ARITHMETIC EVALUATION
*/

red_frac(F, TAB, RES):- red_frac(F, R), write(F), write(' = '), write(R), write(TAB), write(' | correct? '), R == RES, !, write('Yes'), nl.
red_frac(_,   _,   _):- write('No'), nl, false.

sum_deb(A, B, TAB, RES):- eval_sum(A, B, R), write(A), write(' + '), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
sum_deb(_, _,   _,   _):- write('No'), nl, false.

sub_deb(A, B, TAB, RES):- eval_sub(A, B, R), write(A), write(' - '), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
sub_deb(_, _,   _,   _):- write('No'), nl, false.

prod_deb(A, B, TAB, RES):- eval_prod(A, B, R), write(A), write('*'), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
prod_deb(_, _,   _,   _):- write('No'), nl, false.

power_deb(A, B, TAB, RES):- eval_pow(A, B, R), write(A), write('^'), write(B), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
power_deb(_, _,   _,   _):- write('No'), nl, false.

arithm_deb(E, TAB, RES):- arithmetic_eval(E, R), write(E), write(' = '), write(R), write(TAB), write(' | correct? '), RES == R, !, write('Yes'), nl.
arithm_deb(_,   _,   _):- write('No'), nl, false.

debug_numeric:-
	nl, write('-- NUMERICAL DEBUG --'), nl,
	write('- REDUCED FRACTIONS -'), nl, nl,

	write(' 1) '), red_frac(0/1, '     ', 0),
	write(' 2) '), red_frac(0/5, '     ', 0),
	write(' 3) '), red_frac(1/2, '   ', 1/2),
	write(' 4) '), red_frac(4/2, '     ', 2),
	write(' 5) '), red_frac(5/2, '   ', 5/2),
	write(' 6) '), red_frac(6/2, '     ', 3),
	write(' 7) '), red_frac(6/4, '   ', 3/2),
	write(' 8) '), red_frac(6/8, '   ', 3/4),
	write(' 9) '), red_frac(6/10, '  ', 3/5),
	write('10) '), red_frac(-0/1, '     ', 0),
	write('11) '), red_frac(-0/5, '     ', 0),
	write('12) '), red_frac(-1/2, ' ', -1/2),
	write('13) '), red_frac(-4/2, '   ', -2),
	write('14) '), red_frac(-5/2, ' ', -5/2),
	write('15) '), red_frac(-6/2, '   ', -3),
	write('16) '), red_frac(-6/4, ' ', -3/2),
	write('17) '), red_frac(-6/8, ' ', -3/4),
	write('18) '), red_frac(-6/10, '', -3/5),
	
	nl, write('- SUMS -'), nl, nl,

	write(' 1) '), sum_deb(0, 0, '       ', 0),
	write(' 2) '), sum_deb(1, 0, '       ', 1),
	write(' 3) '), sum_deb(4, 0, '       ', 4),
	write(' 4) '), sum_deb(0, 1, '       ', 1),
	write(' 5) '), sum_deb(0, 5, '       ', 5),
	write(' 6) '), sum_deb(1/2, 1/2, '   ', 1),
	write(' 7) '), sum_deb(1/2, 2/2, ' ', 3/2),
	write(' 8) '), sum_deb(1/2, 1, '   ', 3/2),
	write(' 9) '), sum_deb(1, 1/2, '   ', 3/2),
	write('10) '), sum_deb(2/2, 2/2, '   ', 2),
	write('11) '), sum_deb(2/2, 3/2, ' ', 5/2),
	write('12) '), sum_deb(3/2, 1, '   ', 5/2),
	
	nl, write('- SUBS -'), nl, nl,

	write(' 1) '), sub_deb(0, 0, '       ', 0),
	write(' 2) '), sub_deb(1, 0, '       ', 1),
	write(' 3) '), sub_deb(4, 0, '       ', 4),
	write(' 4) '), sub_deb(0, 1, '      ', -1),
	write(' 5) '), sub_deb(0, 5, '      ', -5),
	write(' 6) '), sub_deb(1/2, 1/2, '   ', 0),
	write(' 7) '), sub_deb(1/2, 2/2, '', -1/2),
	write(' 8) '), sub_deb(1/2, 1, '  ', -1/2),
	write(' 9) '), sub_deb(1, 1/2, '   ', 1/2),
	write('10) '), sub_deb(2/2, 2/2, '   ', 0),
	write('11) '), sub_deb(2/2, 3/2, '', -1/2),
	write('12) '), sub_deb(3/2, 1, '   ', 1/2),
	
	nl, write('- PRODS -'), nl, nl,

	write(' 1) '), prod_deb(0, 0, '      ', 0),
	write(' 2) '), prod_deb(1, 0, '      ', 0),
	write(' 3) '), prod_deb(4, 0, '      ', 0),
	write(' 4) '), prod_deb(0, 1, '      ', 0),
	write(' 5) '), prod_deb(0, 5, '      ', 0),
	write(' 6) '), prod_deb(1/2, 1/2, '', 1/4),
	write(' 7) '), prod_deb(1/2, 2/2, '', 1/2),
	write(' 8) '), prod_deb(1/2, 1, '  ', 1/2),
	write(' 9) '), prod_deb(1, 1/2, '  ', 1/2),
	write('10) '), prod_deb(2/2, 2/2, '  ', 1),
	write('11) '), prod_deb(2/2, 3/2, '', 3/2),
	write('12) '), prod_deb(3/2, 1, '  ', 3/2),
	
	nl, write('- POWERS -'), nl, nl,

	write(' 1) '), power_deb(0, 0, '    ', 1),
	write(' 2) '), power_deb(1, 0, '    ', 1),
	write(' 3) '), power_deb(4, 0, '    ', 1),
	write(' 4) '), power_deb(0, 1, '    ', 0),
	write(' 5) '), power_deb(0, 5, '    ', 0),
	write(' 6) '), power_deb(1/2, 2, '', 1/4),
	write(' 7) '), power_deb(1/2, 1, '', 1/2),
	write(' 8) '), power_deb(2, 3, '    ', 8),
	write(' 9) '), power_deb(2/2, 2, '  ', 1),
	write('10) '), power_deb(2/2, 3, '  ', 1),
	write('11) '), power_deb(3/2, 1, '', 3/2),
	write('12) '), power_deb(3/2, 2, '', 9/4),
	
	nl, write('- ARITHMETIC EXPRESSIONS -'), nl, nl,
	write(' 1) '), arithm_deb(3 + 3, '                 ', 6),
	write(' 2) '), arithm_deb(3 - 4, '                ', -1),
	write(' 3) '), arithm_deb(3^4, '                ', 81),
	write(' 4) '), arithm_deb(2^2^2^2, '           ', 256),
	write(' 5) '), arithm_deb(2^2^2^2^2, '       ', 65536),
	write(' 6) '), arithm_deb(2^(1 + 1)^2, '         ', 16),
	write(' 7) '), arithm_deb(2^2^(2 + 1 - 1)^2^2, '', 65536),
	write(' 8) '), arithm_deb((1/2)^2 + 3/4, '         ', 1),
	write(' 9) '), arithm_deb((1/2)^0, '             ', 1),
	write('10) '), arithm_deb((1/2)^0 + 3/4, '       ', 7/4),
	write('11) '), arithm_deb(1 - (1 + 1), '           ', -1),
	write('12) '), arithm_deb(1/2, '               ', 1/2),
	true.

/*
-----------------------------
DEBUG - MONOMIAL EVALUATION
*/

red_mon(M, TAB, RES):- write(M), write(' '), monomial_red(M, R), write(R), write(TAB), write(' | correct? '), R == RES, write('Yes'), nl, !.
red_mon(_,   _,   _):- write('No'), nl, false.

mon_sum(M1, M2, TAB, RES):- write(M1), write(' + '), write(M2), write(' = '), mon_sum(M1, M2, S), write(S), write(TAB), write(' | correct? '), S == RES, write('Yes'), nl, !.
mon_sum( _,  _,   _,   _):- write('No'), nl, false.

mon_sub(M1, M2, TAB, RES):- write(M1), write(' - '), write(M2), write(' = '), mon_sub(M1, M2, S), write(S), write(TAB), write(' | correct? '), S == RES, write('Yes'), nl, !.
mon_sub( _,  _,   _,   _):- write('No'), nl, false.

mon_prod(M1, M2, TAB, RES):- write(M1), write('*'), write(M2), write(' = '), mon_prod(M1, M2, S), write(S), write(TAB), write(' | correct? '), S == RES, write('Yes'), nl, !.
mon_prod( _,  _,   _,   _):- write('No'), nl, false.

debug_monomials:-
	nl, write('-- MONOMIALS DEBUG --'), nl,
	write('- MONOMIAL REDUCTION -'), nl, nl,

	write(' 1) '), red_mon(0*x^0, '                       ', 0),
	write(' 2) '), red_mon(0*x^1, '                       ', 0),
	write(' 3) '), red_mon(0*x^5, '                       ', 0),
	
	write(' 4) '), red_mon(1*x^0, '                       ', 1),
	write(' 5) '), red_mon(1*x^1, '                       ', x),
	write(' 6) '), red_mon(1*x^5, '                     ', x^5),
	
	write(' 7) '), red_mon(3*x^0, '                       ', 3),
	write(' 8) '), red_mon(3*x^1, '                     ', 3*x),
	write(' 9) '), red_mon(3*x^6, '                   ', 3*x^6),

	write('10) '), red_mon((0/1)*x^(0/1), '                ', 0),
	write('11) '), red_mon((0/8)*x^(4/4), '                ', 0),
	write('12) '), red_mon((0/3)*x^(20/4), '               ', 0),

	write('13) '), red_mon((7/7)*x^(0/9), '                ', 1),
	write('14) '), red_mon((6/6)*x^(2/2), '                ', x),
	write('15) '), red_mon((1/1)*x^(5/1), '              ', x^5),

	write('16) '), red_mon((27/9)*x^(0/1), '               ', 3),
	write('17) '), red_mon((18/6)*x^(8/8), '             ', 3*x),
	write('18) '), red_mon((15/5)*x^(36/6), '          ', 3*x^6),

	write('19) '), red_mon(-0*x^0, '                       ', 0),
	write('20) '), red_mon(-(0*x^0), '                   ', 0),
	write('21) '), red_mon(-0*x^1, '                       ', 0),
	write('22) '), red_mon(-(0*x^1), '                   ', 0),
	write('23) '), red_mon(-0*x^5, '                       ', 0),
	write('24) '), red_mon(-(0*x^5), '                   ', 0),
	
	write('25) '), red_mon(-1*x^0, '                     ', -1),
	write('26) '), red_mon(-(1*x^0), '                  ', -1),
	write('27) '), red_mon(-1*x^1, '                     ', -x),
	write('28) '), red_mon(-(1*x^1), '                  ', -x),
	write('29) '), red_mon(-1*x^5, '                   ', -x^5),
	write('30) '), red_mon(-(1*x^5), '                ', -x^5),
	
	write('31) '), red_mon(-3*x^0, '                     ', -3),
	write('32) '), red_mon(-(3*x^0), '                  ', -3),
	write('33) '), red_mon(-3*x^1, '                   ', -3*x),
	write('34) '), red_mon(-(3*x^1), '                ', -3*x),
	write('35) '), red_mon(-3*x^6, '                 ', -3*x^6),
	write('36) '), red_mon(-(3*x^6), '              ', -3*x^6),
	
	write('37) '), red_mon(-((0/1)*x^(0/1)), '            ', 0),
	write('38) '), red_mon((-0/1)*x^(0/1), '                ', 0),
	write('39) '), red_mon((0/1)*x^(-0/1), '                ', 0),
	write('40) '), red_mon((0/1)*x^(-(0/1)), '              ', 0),
	write('41) '), red_mon(-((0/8)*x^(4/4)), '            ', 0),
	write('42) '), red_mon((-0/8)*x^(-4/4), '               ', 0),
	write('43) '), red_mon(-((0/3)*x^(20/4)), '           ', 0),
	write('44) '), red_mon((-0/3)*x^(-20/4), '              ', 0),

	write('45) '), red_mon(-((7/7)*x^(0/9)), '           ', -1),
	write('46) '), red_mon((-7/7)*x^(0/9), '              ', -1),
	write('47) '), red_mon((7/7)*x^(-0/9), '                ', 1),

	write('48) '), red_mon(-((6/6)*x^(2/2)), '           ', -x),
	write('49) '), red_mon((-6/6)*x^(2/2), '              ', -x),
	write('50) '), red_mon((6/6)*x^(-2/2), '           ', x^(-1)),

	write('51) '), red_mon(-((1/1)*x^(5/1)), '         ', -x^5),
	write('52) '), red_mon((-1/1)*x^(5/1), '            ', -x^5),
	write('53) '), red_mon((1/1)*x^(-5/1), '           ', x^(-5)),
	
	write('54) '), red_mon(-((27/9)*x^(0/1)), '          ', -3),
	write('56) '), red_mon((-27/9)*x^(0/1), '             ', -3),
	write('57) '), red_mon((27/9)*x^(-0/1), '               ', 3),

	write('58) '), red_mon(-((18/6)*x^(8/8)), '        ', -3*x),
	write('59) '), red_mon((-18/6)*x^(8/8), '           ', -3*x),
	write('60) '), red_mon((18/6)*x^(-8/7), '    ', 3*x^(-8/7)),

	write('61) '), red_mon(-((15/5)*x^(36/8)), '', -3*x^(9/2)),
	write('62) '), red_mon((-15/5)*x^(36/8), '   ', -3*x^(9/2)),
	write('63) '), red_mon((15/5)*x^(-36/8), '   ', 3*x^(-9/2)),

	write('64) '), red_mon(-(1/2*x), '              ', -1/2*x),
	write('65) '), red_mon(-1/2*x, '                 ', -1/2*x),
	write('66) '), red_mon((-1/2)*x, '                 ', -1/2*x),
	
	nl, write('- MONOMIAL SUM -'), nl, nl,

	write(' 1) '), mon_sum(3*x, 2*x, '        ', 5*x),
	write(' 2) '), mon_sum(3*x, 2*x^0, '    ', 3*x + 2),
	write(' 3) '), mon_sum(3*x, 2*x^1, '      ', 5*x),
	write(' 4) '), mon_sum(3*x^0, 2*x, '    ', 3 + 2*x),
	write(' 5) '), mon_sum(3*x^1, 2*x, '      ', 5*x),
	write(' 6) '), mon_sum(3*x^2, 2*x, '', 3*x^2 + 2*x),
	write(' 7) '), mon_sum(3*x^2, 2*x^2, '  ', 5*x^2),
	write(' 8) '), mon_sum(3*x^0, 2*x^0, '      ', 5),
	write(' 9) '), mon_sum(3*x^0, 0*x^2, '      ', 3),
	write('10) '), mon_sum(0*x^1, 0*x^2, '      ', 0),
	write('11) '), mon_sum(0, 0*x^2, '          ', 0),
	write('12) '), mon_sum(0, 0, '              ', 0),
	
	nl, write('- MONOMIAL SUB -'), nl, nl,

	write(' 1) '), mon_sub(0, x, '             ', -x),
	write(' 2) '), mon_sub(3*x, 2*x, '          ', x),
	write(' 3) '), mon_sub(3*x, 2*x^0, '    ', 3*x - 2),
	write(' 4) '), mon_sub(3*x, 2*x^1, '        ', x),
	write(' 5) '), mon_sub(3*x^0, 2*x, '    ', 3 - 2*x),
	write(' 6) '), mon_sub(3*x^1, 2*x, '        ', x),
	write(' 7) '), mon_sub(3*x^2, 2*x, '', 3*x^2 - 2*x),
	write(' 8) '), mon_sub(3*x^2, 2*x^2, '    ', x^2),
	write(' 9) '), mon_sub(3*x^0, 2*x^0, '      ', 1),
	write('10) '), mon_sub(3*x^0, 0*x^2, '      ', 3),
	write('11) '), mon_sub(0*x^1, 0*x^2, '      ', 0),
	write('12) '), mon_sub(0, 0*x^2, '          ', 0),
	write('13) '), mon_sub(0, 0, '              ', 0),
	
	nl, write('- MONOMIAL PROD -'), nl, nl,

	write(' 1) '), mon_prod(3*x, 2*x, '    ', 6*x^2),
	write(' 2) '), mon_prod(3*x, 2*x^0, '    ', 6*x),
	write(' 3) '), mon_prod(3*x, 2*x^1, '  ', 6*x^2),
	write(' 4) '), mon_prod(3*x^0, 2*x, '    ', 6*x),
	write(' 5) '), mon_prod(3*x^1, 2*x, '  ', 6*x^2),
	write(' 6) '), mon_prod(3*x^2, 2*x, '  ', 6*x^3),
	write(' 7) '), mon_prod(3*x^2, 2*x^2, '', 6*x^4),
	write(' 8) '), mon_prod(3*x^0, 2*x^0, '    ', 6),
	write(' 9) '), mon_prod(3*x^0, 0*x^2, '    ', 0),
	write('10) '), mon_prod(0*x^1, 0*x^2, '    ', 0),
	write('11) '), mon_prod(0, 0*x^2, '        ', 0),
	write('12) '), mon_prod(0, 0, '            ', 0),

	true.