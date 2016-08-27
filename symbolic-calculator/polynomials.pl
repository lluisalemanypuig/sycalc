% POLYNOMIALS
% A polynomial is a sum of monomials.
% These are monomials:
% x^3, -2*x, 6*x^2, x, -2
% These are not monomials
% (x + 3)*(x - 2)


polynomial_monomials(M, [M]):- monomial(M), !.
polynomial_monomials(A + B, [B|L]):- monomial(B), polynomial_monomials(A, L), !.
polynomial_monomials(A - B, [-B|L]):- monomial(B), polynomial_monomials(A, L), !.
polynomial_monomials(E, _):-
	write('Error (polynomial_monomials): error when parsing monomials. Monomial received: '),
	write(E), nl, false.

