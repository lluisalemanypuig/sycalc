:-ensure_loaded(number).
:-ensure_loaded(list).

% MONOMIALS

/***
	@descr This file contains the definition of a monomial and the
	predicates necessary for their simple manipulation. A monomial
	consists of a series of variables multiplied by a reduced numerical
	value (at the left of all the variables). The variables may have
	an exponent which is also a numerical rational value (at the right
	of the variable). The coefficient or the exponent may be inexistent
	(= 1), and so can the variables. A reduced numerical value is the
	result of arith_expr_eval(E) where E is an arithmetic expression.
	Attention: if the exponent or coeficient are rational, it is better
	to use parentheses.
	
	
	Henceforth, "unimonomial" will be used as a synonym for
	"univariate monomial".
*/

/*! Definition of monomial. */

/**
	@form monomial(Value)
	@descr This predicate fails if @Value is not a monomial. A monomial
	is something we can extract monomial components from. The components
	are:
	
	<++
	!> a rational number as coefficient
	!> a product of variables each one raised to a rational value
	!> nothing else is a component
	++>
	
	(the coefficient or any of the exponents may be written as
	arithmetic expressions, e.g., 3*3, 2+1, ...)
*/
monomial(M):-
	monomial_comps(M, C,Vs,Es),
	rational(C), inspection(atom,Vs), inspection(rational,Es).

% Here we distinguish between two types of monomials: simple and
% complex monomials.
% 	- Simple monomials are univariate, that is, have only one
% 	variable. A single monomial may be
% 		* full: coeficient, variable and exponent
% 		* partial: a subset of two of the above three elements
% 		(note that the combination 'coeficient, exponent' is the
% 		same as only coeficient)
% 		* degenerate: only of the three elements above (either the
% 		coeficient or the variable)
% 	- Complex monomials are multivariate, that is, have two or more
% 	variables.

% any negated monomial
monomial_comps_(- (M), NC, V, E):-
	monomial_comps_(M, C, V, E), rational_neg(C, NC), !.

% Simple monomial: full
monomial_comps_(C*V^E, CE, [V], [EE]):-
	arith_expr_eval(C, CE), arith_expr_eval(E, EE), !.

% Simple monomial: partial (variable, exponent)
monomial_comps_(X^E, 1, [X], [EE]):-
	atom(X), arith_expr_eval(E, EE), !.

% Simple monomial: partial (coeficient, variable)
monomial_comps_(C*X, CE, [X], [1]):-
	atom(X), arith_expr_eval(C, CE), !.

% Simple monomial: degenerate (coeficient)
monomial_comps_(C, CE, [], []):-
	arith_expr_eval(C, CE), !.

% Simple monomial: degenerate (variable)
monomial_comps_(X, 1, [X], [1]):-
	not(expr(X)), !.

% Complex monomial:
%    coeficient * (variable^exponent) * ... * (variable^exponent)
%    Last is: variable^exponent
monomial_comps_(S*V^E, C, [V|Vs], [EE|Es]):-
	arith_expr_eval(E, EE), monomial_comps_(S, C,Vs,Es), !.
%    Last is variable
monomial_comps_(S*V, C, [V|Vs], [1|Es]):-
	monomial_comps_(S, C,Vs,Es), !.

/*! Components of a monomial. */

/**
	@form monomial_comps(Monomial, Coefficient,Variables,Exponents)
	@descr Obtains the coefficient, list of variables and exponents of
	the monomial.
	
	The coefficients and exponents are given in reduced form:
	<--
	(2 + 2)*x^(3 - 1) gives: C = 4, V = [x], E = [2]
	-->
	@constrs 
		@param Coefficient The rational value that multiplies all the
		variables
		@param Variables The variables of the monomial
		@param Exponents The rational values each variable is raised to.
		The i-th value of this list is the exponent of the i-th variable
		in @Variables.
*/
monomial_comps(M, C,V,E):-
	monomial_comps_(M,C,Vs,Es),
	reverse(Vs,V), reverse(Es,E).

/**
	@form monomial_neg(Monomial, NegMonomial)
	@descr @NegMonomial is a monomial equal to (-1)*@Monomial.
*/
monomial_neg(M, N):-
	monomial_comps(M, C,V,E), rational_neg(C, CN),
	red_monomial_from_comps(CN,V,E, N).

/**
	@form monomial_coefficient(Monomial, Value)
	@descr @Value is the rational value that multiplies the variables of
	@Monomial (the coefficient).
*/
monomial_coefficient(M, C):- monomial_comps(M, C, _, _).

/**
	@form monomial_var_exp_comps(Var, Vars,Exps, RestVars,RestExps, Exp)
	@descr Find the exponent of variable @Var in @Exps. If @Var is a member
	of @Exp then the value it takes is taken from @Exps. If @Var is not
	a member then @Exp is 0.
	@constrs
		@param Var The variable whose exponent we want.
		@param Vars The list of variables of the monomial.
		@param Exps The exponents of the variables in @Vars.
		@param RestVars The list of variables without @Var.
		@param RestExps The list of exponents without @Var 's.
*/
monomial_var_exp_comps(_,     [],     [],      _,     _, 0):- !.
monomial_var_exp_comps(V, [V|Vs], [E|Es],     Vs,    Es, E):- !.
monomial_var_exp_comps(V, [X|Vs], [P|Es], [X|Vr],[P|Er], E):-
	monomial_var_exp_comps(V, Vs,Es, Vr,Er,E).

/**
	@form monomial_var_exp(Monomial, Var, Var^Exp)
	@descr Finds the exponent of variable @Var. Uses predicate 
	?monomial_var_exp_comps/6.
*/
monomial_var_exp(M, V, V^E):-
	monomial_comps(M, _,Vs,Es),
	monomial_var_exp_comps(V, Vs,Es, _,_, E).

/**
	@form monomial_degrees(Monomial, Degrees)
	@descr Obtain the exponent of each of the variables of @Monomial.
*/
monomial_degrees(M, []):- monomial_comps(M, _, [],[]), !.
monomial_degrees(M, D):- monomial_comps(M, _, _, D).

/*! Reduction of monomials. */

% Joins in a nice way variables raised to an exponent multiplying a list
% of variables, each probably raised to another exponent. Variants: put
% a 'minus' sign at the beginning

/**
	@form neg_monomial_vars_product(Vars, NegVars)
	@descr @NegVars is the set of variables @Vars equal to (-1)*@Vars.
	@constrs @Vars cannot have a leading coefficient equal to -1.
*/
neg_monomial_vars_product(RV, R):-
	nonvar(RV),									% check RV is instanciated
	term_string(RV, RVs),
	string_concat("-", RVs, SS),
	term_string(R, SS), !.
neg_monomial_vars_product(_, _).

/**
	@form monomial_vars_product(Var, Vars2, NegVars)
	@descr @NegVars is the set of variables equal to (-1)*@Var*@Vars2.
	@constrs @Var and @Vars2 cannot have a leading coefficient equal to -1.
*/
monomial_vars_product(X, RV, R):-
	nonvar(RV),									% check RV is instanciated
	term_string(X, Xs),							% x as string
	string_concat(Xs, "*", F),					% 'x*' as string
	term_string(RV, RVs),						% reduced variables as string
	string_concat(F, RVs, SS),					% join strings
	term_string(R, SS), !.						% convert result to term
monomial_vars_product(X, _, X).

/**
	@form monomial_vars_product(Var, Exp, Vars2, NegVars)
	@constrs Var and Vars2 cannot have a leading coefficient equal to -1.
	@descr NegVars is the set of variables Vars equal to (-1)*(Var^E)*vars2.
*/
monomial_vars_product(X, E, RV, R):-
	nonvar(RV),									% check RV is instanciated
	term_string(X, Xs),							% x as string
	term_string(E, Es),							% exponent as string
	atomic_list_concat([Xs,"^",Es, "*"], F),	% 'x^c*' as string
	term_string(RV, RVs),						% reduced variables as string
	string_concat(F, RVs, SS),					% join strings
	term_string(R, SS), !.						% term as string
monomial_vars_product(X, E, _, X^E).

% MONOMIAL REDUCTION

/**
	@form collapse_vars_list(Vars,Exps, NewVars,NewExps)
	@constrs Vars is a lexicographically sorted list of variables.
	The i-th element of Exps is the exponent of the i-th variable in
	Vars. Vars and Exps must have the same length.
	@descr NewVars and NewExps are the contraction of lists Vars and Exps
	guided by Vars: two equal variables in consecutive positions in Vars
	are collapsed into a single element and the exponents are added. For
	example:
	<--
		[i, i, j, k,k, k, k, z],
		[1,-1, 3, 1,1,-1,-1, 4]
	-->
	gives
	<--
		[i, j, k, z]
		[0, 3, 0, 4]
	-->
*/
collapse_vars_list(        [],        [],     [],  []):- !.
collapse_vars_list(       [V],       [E],    [V], [E]):- !.
collapse_vars_list(   [V1,V1],   [E1,E2],   [V1],[SE]):- arith_expr_eval(E1 + E2, SE), !.
collapse_vars_list(   [V1,V2],	      Es,[V1,V2],  Es):- !.
collapse_vars_list([V1,V1|Xs],[E1,E2|Es],    CVs, CEs):-
	arith_expr_eval(E1 + E2, SE),
	collapse_vars_list([V1|Xs],[SE|Es], CVs, CEs), !.
collapse_vars_list([V1,V2|Xs],[E1,E2|Es], [V1|CVs],[E1|CEs]):-
	collapse_vars_list([V2|Xs],[E2|Es], CVs,CEs).

/**
	@form red_monomial_comps(Coef,Vars,Exps, Rcoef,Rvars,Rexps)
	@descr Rcoef,Rvars and Rexps are the reduced coefficient, reduced
	variables and reduced exponents of the monomial defined by Coef,
	Vars and Exps. A reduced coefficient is the result of the evaluation
	of the arithmetic expression. The reduced variables are the list of
	variables without those that have null exponent, and, for example,
	if a variable appears twice, the exponents for each of its occurrences
	are added up (see collapse_vars_list).
*/
red_monomial_comps(0, _, _,  0, [], []):- !.
red_monomial_comps(C,Vs,Es, Rc,RVs,REs):-
	arith_expr_eval(C,Rc),
	collapse_vars_list(Vs,Es, CVs,CEs),
	pdrop(0, CEs,CVs, REs,RVs).

/**
	@form red_vars_monomial(Vars,Exps, Monomial)
	@descr Monomial is the product of the variables each raised to the
	corresponding power in Exps. The resulting monomial is reduced: if
	any variable has exponent 0 then it is not included in the product.
	If the variable has exponent 1, the exponent does not appear in the
	product. For example:
	<--
		[i,j,k,l], [0,1,2,3]
	-->
	gives
	<--
		j*k^2*l^3
	-->
*/
red_vars_monomial(    [],  [], _):- !.
red_vars_monomial(   [_], [0], _):- !.
red_vars_monomial(   [X], [1], X):- !.
red_vars_monomial(   [X], [E], X^EE):- arith_expr_eval(E, EE), !.
red_vars_monomial([_|Xs], [0|Es], RV):- red_vars_monomial(Xs, Es, RV), !.
red_vars_monomial([X|Xs], [1|Es], R):-
	red_vars_monomial(Xs, Es, RV),
	monomial_vars_product(X, RV, R), !.
red_vars_monomial([X|Xs], [E|Es], R):-
	arith_expr_eval(E, EE),
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
	arith_expr_eval(C, CE),
	monomial_vars_product(CE, RV, MR), !.

/**
	@form red_monomial_from_comps(Coef,Vars,Exps, Monomial)
	@descr Monomial is a reduced monomial obtained from a list of
	monomial components that need not be reduced.
*/
red_monomial_from_comps(C,V,E, R):-
	arith_expr_eval(C, CE), map(arith_expr_eval, E, EE),
	red_monomial_comps(CE,V,EE, Rc,RVs,REs),
	pisort(RVs,REs, SVs,SEs),
	red_monomial__(Rc,SVs,SEs, R).

/**
	@form red_monomial_from_comps(Monomial, RedMonomial)
	@descr RedMonomial is a reduced monomial equal to Monomial.
*/
red_monomial(M, R):-
	monomial_comps(M, C,V,E),
	pisort(V,E, SV,SE),
	red_monomial_from_comps(C,SV,SE, R).

/**
	@form monomial_positive_coefficient(Monomial)
	@descr Predicate fails if the monomial has a negative (< 0) coefficient.
*/
monomial_positive_coefficient(M):-
	monomial_comps(M, C, _, _), C >= 0.

/**
	@form monomial_revar(OldVar,NewVar, Monomial, Replaced)
	@descr Replaced is a monomial with the same coefficient and variables
	as Monomial, except for variable OldVar which is replaced by NewVar.
*/
monomial_revar(VAR,I, M, R):-
	monomial_comps(M, C,V,E),
	replace(VAR, I, V, Rv),
	pisort(Rv,E, Vs,Es),
	red_monomial_from_comps(C,Vs,Es, R).

% Vo is the variable O with its exponent
% Wo is the rest of the monomial M
% (if monomial M does not have variable O, Vo is 1)
monomial_split_(  _, M,_,[],[], 1, M):- nl, !.
monomial_split_(VAR, M, _,V, E, 1, M):- psplit(VAR, V,E, [],[], V,E), !.
monomial_split_(VAR, _, C,V, E, Vo,Wo):-
	psplit(VAR, V,E, Os,Oes, Vs,Es),
	red_monomial_from_comps(1,Os,Oes, Vo),
	red_monomial_from_comps(C,Vs,Es, Wo), !.

/**
	@form monomial_split(Var, Monomial, VariableExponent, Rest)
	@descr @VariableExponent is the monomial consisting of variable @Var
	and its exponent, and @Rest is the rest of @Monomial. The product of
	@VariableExponent and @Rest is equal to @Monomial.
	Splitting
	<--
		3*x*y^2*z
	-->
	at y gives
	<--
		VariableExponent= y^2, 
		Rest= 3*x*z
	-->
*/
monomial_split(VAR, M, Vo,Wo):-
	monomial_comps(M, C,V,E),
	monomial_split_(VAR, M,C,V,E, Vo,Wo).

/*! Predicates for the comparison of monomials. */

/**
	@form monomial_components_comp(Coef1,Vars1,Exps1, Coef2,Vars2,Exps2)
	@descr Compares the monomials whose components are Coef1,Vars1,Exps1
	and Coef2,Vars2,Exps2 and fails if the comparison '@<' is false. This
	comparison is defined as:
	* If both monomials have no variables then sort using coefficient.
	* If one of the monomials does not have any variable, that monomial goes first
	* If the variables are equal and exponents are equal, sort by coefficient
	* If the variables are equal and exponents are different, sort by exponent
	* If the variables are different then take the lengths
		-> lexicographically sort the variables lengths are equal
		-> else sort by length
*/
monomial_components_comp(C1,[],[], C2,[],[]):- !, C1 > C2.
monomial_components_comp( _,[],[],  _, _, _):- !, false.
monomial_components_comp( _, _, _,  _,[],[]):- !, true.
monomial_components_comp(C1,V1,E1, C2,V1,E1):- !, C1 > C2.
monomial_components_comp( _,V1,E1,  _,V1,E2):- !, E1 @> E2.
monomial_components_comp( _,V1, _,  _,V2, _):-
	length(V1,L1), length(V2,L1), !, V1 @< V2.
monomial_components_comp( _,V1, _,  _,V2, _):-
	length(V1,L1), length(V2,L2), L1 > L2.

/**
	@form monomial_comp(Monomial1, Monomial2)
	@descr Compares the monomials Monomial1 and Monomial2. This predicate
	fails if it is not true that Monomial1 < Monomial2.
*/
monomial_comp(M1, M2):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	monomial_components_comp(C1,V1,E1, C2,V2,E2).

/**
	@form monomial_inv_comp(Monomial1, Monomial2)
	@descr Compares the monomials Monomial1 and Monomial2. This predicate
	fails if it is not true that Monomial1 > Monomial2.
*/
monomial_inv_comp(M1, M2):-
	monomial_comps(M1, C1,V1,E1),
	monomial_comps(M2, C2,V2,E2),
	monomial_components_comp(C2,V2,E2, C1,V1,E1).

/**
	@form monomial_sort(List, SortedList)
	@constrs List is a list of monomials.
	@descr SortedList has the same elements as List but each pair of
	elements in it meet the precedence relationship of '@<'
*/
monomial_sort(L, R):- isort_by(monomial_comp, L, R).

/**
	@form monomial_inv_sort(List, SortedList)
	@constrs List is a list of monomials.
	@descr SortedList has the same elements as List but each pair of
	elements in it meet the precedence relationship of '@>'
*/
monomial_inv_sort(L, R):- isort_by(monomial_inv_comp, L, R).

/*! The following predicates are restricted to unimonomials only. */

/**
	@form unimonomial_degree(Monomial, Degree)
	@descr @Degree is the degree of @Monomial, that is, the exponent of
	the only variable.
	@constrs 
		@param Monomial A univariate monomial.
*/
unimonomial_degree(M, 0):- monomial_comps(M, _,[],[]), !.
unimonomial_degree(M, D):- monomial_comps(M, _,_,Ds), !, first(Ds, D, _).
