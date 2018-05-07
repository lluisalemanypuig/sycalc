output_text(OBJ, EXP):- write(OBJ), write(' | Not correct - Expected to see: '), write(EXP), nl, false.
output_correct(I):- write('  '), write(I), write(' Ok'), nl.

all_to_string([space], ' '):- !.
all_to_string([X], S):- term_to_atom(X, S), !.
all_to_string([space|L], S):- all_to_string(L, LS), atom_concat(' ', LS, S), !.
all_to_string([X|L], S):- term_to_atom(X, AX), all_to_string(L, LS), atom_concat(AX, LS, S), !.
