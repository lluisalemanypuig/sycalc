:-ensure_loaded(core).
:-ensure_loaded(algorithms).

main(Argv):-
	[_,Nn,Mm] = Argv,
	
	write("Nn: "), write(Nn), nl,
	write("Mm: "), write(Mm), nl,
	
	string_codes(Nn, An),	number_string(N, An),
	string_codes(Mm, Am),	number_string(M, Am),
	
	write("N: "), write(N), nl,
	write("M: "), write(M), nl,
	
	P00 = 144*choose(n,4)*choose(m,4),
	P24 = 2*choose(n,2)*choose(m,2),
	P13 = 12*choose(n,3)*choose(m,2) + 12*choose(n,2)*choose(m,3),
	P12 = 36*choose(n,3)*choose(m,3),
	P04 = 2*choose(n,2)*choose(m,2),
	P03 = 12*choose(n,3)*choose(m,2) + 12*choose(n,2)*choose(m,3),
	P021 = 72*choose(n,3)*choose(m,3),
	P022 = 24*choose(n,2)*choose(m,4) + 24*choose(n,4)*choose(m,2) + 36*choose(n,3)*choose(m,3),
	P01 = 144*choose(n,4)*choose(m,3) + 144*choose(n,3)*choose(m,4),
	
	polynomial_expression_evaluation(144*choose(N,4)*choose(M,4), F00),
	polynomial_expression_evaluation(2*choose(N,2)*choose(M,2), F24),
	polynomial_expression_evaluation(12*choose(N,3)*choose(M,2) + 12*choose(N,2)*choose(M,3), F13),
	polynomial_expression_evaluation(36*choose(N,3)*choose(M,3), F12),
	polynomial_expression_evaluation(2*choose(N,2)*choose(M,2), F04),
	polynomial_expression_evaluation(12*choose(N,3)*choose(M,2) + 12*choose(N,2)*choose(M,3), F03),
	polynomial_expression_evaluation(72*choose(N,3)*choose(M,3), F021),
	polynomial_expression_evaluation(24*choose(N,2)*choose(M,4) + 24*choose(N,4)*choose(M,2) + 36*choose(N,3)*choose(M,3), F022),
	polynomial_expression_evaluation(144*choose(N,4)*choose(M,3) + 144*choose(N,3)*choose(M,4), F01),
	
	polynomial_expression_evaluation(P00, EP00),
	polynomial_expression_evaluation(P24, EP24),
	polynomial_expression_evaluation(P13, EP13),
	polynomial_expression_evaluation(P12, EP12),
	polynomial_expression_evaluation(P04, EP04),
	polynomial_expression_evaluation(P03, EP03),
	polynomial_expression_evaluation(P021, EP021),
	polynomial_expression_evaluation(P022, EP022),
	polynomial_expression_evaluation(P01, EP01),
	
	write('f00=  '), write(EP00), nl,
	write('f24=  '), write(EP24), nl,
	write('f13=  '), write(EP13), nl,
	write('f12=  '), write(EP12), nl,
	write('f04=  '), write(EP04), nl,
	write('f03=  '), write(EP03), nl,
	write('f021= '), write(EP021), nl,
	write('f022= '), write(EP022), nl,
	write('f01=  '), write(EP01), nl,
	
	polynomial_expression_evaluation(P00 + P24 + P13 + P12 + P04 + P03 + P021 + P022 + P01, S),
	polynomial_expression_evaluation( P24^2, Q2),
	
	write('  S=  '), write(S), nl,
	write('Q^2=  '), write(Q2), nl,
	
	nl,
	write('Is the sum of frequencies equal to Q^2?'), nl,
	polynomial_eq(S,Q2),
	write('    Yes!'), nl,
	nl,
	
	write('f00=  '), write(F00), nl,
	write('f24=  '), write(F24), nl,
	write('f13=  '), write(F13), nl,
	write('f12=  '), write(F12), nl,
	write('f04=  '), write(F04), nl,
	write('f03=  '), write(F03), nl,
	write('f021= '), write(F021), nl,
	write('f022= '), write(F022), nl,
	write('f01=  '), write(F01), nl,
	
	polynomial_expression_evaluation(
		  (2/9 + 1/9*(N+M-4))*choose(N,2)*choose(M,2)
		+ 1/5*choose(N,3)*choose(M,3)
		+ 2/15*choose(N,2)*choose(M,4)
		+ 2/15*choose(N,4)*choose(M,2),
		VAR
	),
	write('VAR=  '), write(VAR), nl,
	
	halt.
main(Argv):-
	write(Argv), nl,
	write('Error!'), nl,
	write('    Usage of program is: ./freq-comp-bip n m'), nl,
	write('    where n and m are integers'), nl,
	halt.
