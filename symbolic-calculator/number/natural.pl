% NATURALS

zero(0).
one(1).

natural(N):- integer(N), N >= 0.

next_natural(0).
next_natural(N):- next_natural(M), N is M + 1.