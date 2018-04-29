:-ensure_loaded(number).
:-ensure_loaded(list).

% MONOMIALS

% A monomial consists of a series of variables multiplied by a reduced
% numerical value (at the left of all the variables). The variables may
% have an exponent which is also a numerical rational value (at the
% right of the variable). The coefficient or the exponent may be
% inexistent (= 1), and so can the variables. A reduced numerical value
% is the result of arithmetic_eval(E) where E is an arithmetic
% expression.

% Here we distinguish between two types of monomials: simple and complex
% monomials.
% - Single monomials are univariate, that is, have only one variable. A
% single monomial may be
%    * full: coeficient, variable and exponent
%    * partial: a subset of two of the above three elements
%	 (note that the combination 'coeficient, exponent' is the same
%	 as only coeficient)
%    * degenerate: only of the three elements above (either the
%        coeficient or the variable)
% - Complex monomials are multivariate, that is, have two or more
% variables.

% Attention: if the exponent or coeficient are rational, use parentheses

% monomial_comps: Extracts the components of a monomial in reduced
% form: (2 + 2)*x^(3 - 1) gives: C = 4, V = x, E = 2

% any negated monomial
monomial_comps_(- (M), NC, V, E):-
	monomial_comps_(M, C, V, E),
	rational_neg(C, NC), !.

% Simple monomial: full
monomial_comps_(C*V^E, CE, [V], [EE]):-
	arithmetic_eval(C, CE),
	arithmetic_eval(E, EE), !.

% Simple monomial: partial (variable, exponent)
monomial_comps_(X^E, 1, [X], [EE]):- atom(X), arithmetic_eval(E, EE), !.
% Simple monomial: partial (coeficient, variable)
monomial_comps_(C*X, CE, [X], [1]):- atom(X), arithmetic_eval(C, CE), !.
% Simple monomial: degenerate (coeficient)
monomial_comps_(C, CE, [], []):- arithmetic_eval(C, CE), !.
% Simple monomial: degenerate (variable)
monomial_comps_(X, 1, [X], [1]):- not(expr(X)), !.
% Complex monomial:
%    coeficient * (variable^exponent) * ... * (variable^exponent)
%    Last is: variable^exponent
monomial_comps_(S*V^E, C, [V|Vs], [EE|Es]):-
	arithmetic_eval(E, EE),
	monomial_comps_(S, C,Vs,Es),
	!.
%    Last is variable
monomial_comps_(S*V, C, [V|Vs], [1|Es]):-
	monomial_comps_(S, C,Vs,Es), !.

monomial_comps(M, C,V,E):-
	monomial_comps_(M,C,Vs,Es),
	reverse(Vs,V), reverse(Es,E).

% Monomial definition: a monomial is something we can extract monomial
% components from. The components are:
% -> a rational number as coefficient
% -> a product of variable raised to a rational number
% -> nothing else is a component
% (the rational number in the coefficient or any of the exponents can be
% written as an arithmetic expression, e.g., 3*3, 2+1, ...)
monomial(M):-
	monomial_comps(M, C,Vs,Es),
	rational(C),
	inspection(atom,Vs),
	inspection(rational, Es).

% N = -M, where M is a monomial
monomial_neg(M, N):-
	monomial_comps(M, C,V,E), rational_neg(C, CN),
	red_monomial_from_comps(CN,V,E, N).

% C is the coefficient of the monomial M
monomial_coefficient(M, C):- monomial_comps(M, C, _, _).

% E is the exponent of variable V, if such variable exists,
% extracted from the list of variables and corresponding exponents.
% Vr and Er are the other variables and exponents, in the same
% original order. If it does not exist, then the exponent is 0.
monomial_exponent(_,     [],     [],      _,     _, 0):- !.
monomial_exponent(V, [V|Vs], [E|Es],     Vs,    Es, E):- !.
monomial_exponent(V, [X|Vs], [P|Es], [X|Vr],[P|Er], E):-
	monomial_exponent(V, Vs,Es, Vr,Er,E).

% The exponent E of a variable V in the monomial V
monomial_var_exp(M, V, V^E):-
	monomial_comps(M, _,Vs,Es),
	monomial_exponent(V, Vs,Es, _,_, E).

% D are the exponents of the monomial M
monomial_degrees(M, []):- monomial_comps(M, _, [],[]), !.
monomial_degrees(M, D):- monomial_comps(M, _, _, D).

unimonomial_degree(M, 0):- monomial_comps(M, _,[],[]), !.
unimonomial_degree(M, D):- monomial_comps(M, _,_,Ds), !, first(Ds, D, _).

% Monomial reduction

% Joins in a nice way variables raised to an exponent multiplying a list
% of variables, each probably raised to another exponent. Variants: put
% a 'minus' sign at the beginning
neg_monomial_vars_product(RV, R):-
	nonvar(RV),
	term_string(RV, RVs),
	string_concat("-", RVs, SS),
	term_string(R, SS), !.
neg_monomial_vars_product(_, _).
monomial_vars_product(X, RV, R):-
	nonvar(RV),                               % check RV is instanciated
	term_string(X, Xs),                       % x as string
	string_concat(Xs, "*", F),		  % 'x*' as string
	term_string(RV, RVs),                     % reduced variables as string
	string_concat(F, RVs, SS),                % join strings
	term_string(R, SS), !.                    % convert result to term
monomial_vars_product(X, _, X).
monomial_vars_product(X, E, RV, R):-
	nonvar(RV),                               % check RV is instanciated
	term_string(X, Xs),			  % x as string
	term_string(E, Es),                       % exponent as string
	atomic_list_concat([Xs,"^",Es, "*"], F),  % 'x^c*' as string
	term_string(RV, RVs),                     % reduced variables as string
	string_concat(F, RVs, SS),		  % join strings
	term_string(R, SS), !.                    % term as string
monomial_vars_product(X, E, _, X^E).

% Obtains the reduced list of variables (drops those variables with null
% exponent, i.e., exponent equal 0) and coefficient (if it is 0, the
% list of variables and exponents are empty)
collapse_vars_list(        [],        [],     [],  []):- !.
collapse_vars_list(       [V],       [E],    [V], [E]):- !.
collapse_vars_list(   [V1,V1],   [E1,E2],   [V1],[SE]):- arithmetic_eval(E1 + E2, SE), !.
collapse_vars_list(   [V1,V2],	      Es,[V1,V2],  Es):- !.
collapse_vars_list([V1,V1|Xs],[E1,E2|Es],    CVs, CEs):-
	arithmetic_eval(E1 + E2, SE),
	collapse_vars_list([V1|Xs],[SE|Es], CVs, CEs), !.
collapse_vars_list([V1,V2|Xs],[E1,E2|Es], [V1|CVs],[E1|CEs]):-
	collapse_vars_list([V2|Xs],[E2|Es], CVs,CEs).

red_monomial_vars_list(    [],    [],      [],     []):- !.
red_monomial_vars_list([_|Vs],[0|Es],     RVs,    REs):- red_monomial_vars_list(Vs,Es, RVs,REs), !.
red_monomial_vars_list([V|Vs],[E|Es], [V|RVs],[E|REs]):- red_monomial_vars_list(Vs,Es, RVs,REs).
red_monomial_comps(0, _, _,  0, [], []):- !.
red_monomial_comps(C,Vs,Es, Rc,CVs,CEs):-
	arithmetic_eval(C,Rc),
	red_monomial_vars_list(Vs,Es, RVs, REs),
	collapse_vars_list(RVs,REs, CVs,CEs).

% Obtains a reduced monomial from a non-reduced list of variables
% without coefficient (or coefficient = 1)
red_vars_monomial(    [],  [], _):- !.
red_vars_monomial(   [_], [0], _):- !.
red_vars_monomial(   [X], [1], X):- !.
red_vars_monomial(   [X], [E], X^EE):- arithmetic_eval(E, EE), !.
red_vars_monomial([_|Xs], [0|Es], RV):- red_vars_monomial(Xs, Es, RV), !.
red_vars_monomial([X|Xs], [1|Es], R):-
	red_vars_monomial(Xs, Es, RV),
	monomial_vars_product(X, RV, R), !.
red_vars_monomial([X|Xs], [E|Es], R):-
	arithmetic_eval(E, EE),
	red_vars_monomial(Xs, Es, RV),
	monomial_vars_product(X, EE, RV, R), !.

% Obtains a reduced monomial from a list of components
red_monomial__( 0, _, _, 0):- !.
red_monomial__( 1, V, E, RV):-
	red_vars_monomial(V,E, RV), nonvar(RV), !.
red_monomial__( 1, _, _,  1):- !.
red_monomial__(-1, [], [], -1):- !.
red_monomial__(-1, V, E, MR):-
	red_vars_monomial(V,E, RV),
	neg_monomial_vars_product(RV, MR), !.
red_monomial__( C, V, E, MR):-
	red_vars_monomial(V,E, RV),
	arithmetic_eval(C, CE),
	monomial_vars_product(CE, RV, MR), !.

% R is the reduction of the monomial C*(V_i)^(E_i)
red_monomial_from_comps(C,V,E, R):-
	arithmetic_eval(C, CE), map(arithmetic_eval, E, EE),
	red_monomial_comps(CE,V,EE, Rc,RVs,REs),
	pisort(RVs,REs, SVs,SEs),
	red_monomial__(Rc,SVs,SEs, R).

% R is the reduction of the monomial M
red_monomial(M, R):-
	monomial_comps(M, C,V,E),
	pisort(V,E, SV,SE),
	red_monomial_from_comps(C,SV,SE, R).

% Monomial comparison (by components)
% If both monomials have no variables then sort using coefficient.
% If one of the monomials does not have any variable, that monomial goes first
% If the variables are equal and exponents are equal, sort by coefficient
% If the variables are equal and exponents are different, sort by exponent
% If the variables are different then take the lengths
%   -> lexicographically sort the variables lengths are equal
%   -> else sort by length
monomial_components_comp(C1,[],[], C2,[],[]):- !, C1 > C2.
monomial_components_comp( _,[],[],  _, _, _):- !, false.
monomial_components_comp( _, _, _,  _,[],[]):- !, true.
monomial_components_comp(C1,V1,E1, C2,V1,E1):- !, C1 > C2.
monomial_components_comp( _,V1,E1,  _,V1,E2):- !, E1 @> E2.
monomial_components_comp( _,V1, _,  _,V2, _):-
	length(V1,L1), length(V2,L1), !, V1 @< V2.
monomial_components_comp( _,V1, _,  _,V2, _):-
	length(V1,L1), length(V2,L2), L1 > L2.

monomial_comp(M1, M2):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	monomial_components_comp(C1,V1,E1, C2,V2,E2).

monomial_inv_comp(M1, M2):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	monomial_components_comp(C2,V2,E2, C1,V1,E1).

% Sort a list of monomials L into R using different comparisons
monomial_sort(L, R):- isort_by(monomial_comp, L, R).
monomial_inv_sort(L, R):- isort_by(monomial_inv_comp, L, R).

% Does this monomial has a positive coefficient?
monomial_positive_coefficient(M):-
	monomial_comps(M, C, _, _), C >= 0.

% Replace the variable O with variable I
monomial_revar(O,I, M, R):-
	monomial_comps(M, C,V,E),
	drop_with(O, I, V, Rv),
	pisort(Rv,E, Vs,Es),
	red_monomial_from_comps(C,Vs,Es, R).

% Vo is the variable O with its exponent
% Wo is the rest of the monomial M
% (if monomial M does not have variable O, Vo is 1)
monomial_split_(O, M,_,V,E, 1, M):- psplit_at(O, V,E, [],[], _,_), !.
monomial_split_(O, _,C,V,E, Vo,Wo):-
	psplit_at(O, V,E, Os,Oes, Vs,Es),
	red_monomial_from_comps(1,Os,Oes, Vo),
	red_monomial_from_comps(C,Vs,Es, Wo), !.

monomial_split(O, M, Vo,Wo):-
	monomial_comps(M, C,V,E),
	monomial_split_(O, M,C,V,E, Vo,Wo).
