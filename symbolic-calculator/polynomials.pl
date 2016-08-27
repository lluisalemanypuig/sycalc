% POLYNOMIALS
% a polynomial is a sum of monomials

polynomial_monomials([], []):- !.
polynomial_monomials([M + L], [M|R]):- polynomial_monomials(L, R), !.
polynomial_monomials([M - L], [M|L]):- polynomial_monomials(L, [F|R]), L is [-F|R], !.
polynomial_monomials([M], [M]).
