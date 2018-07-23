/***
	@descr This file contains a number of simple sorting algorithms,
	and two "list-fusion" algorithms.
*/

/**
	@form insert_by(Function, Value, List, NewList)
	@descr Inserts @Value into @List according to @Function using the
	insertion sort algorithm.
*/
insert_by(_, X, [], [X]):- !.
insert_by(F, X, [Y|L], [X,Y|L]):- call(F, X, Y), !.   % X < Y
insert_by(F, X, [Y|L], [Y|R]):- insert_by(F, X, L, R).

/**
	@form isort_by(Function, List, SortedList)
	@descr Insertion sort according guided by @Function.
*/
isort_by(_, [], []):- !.
isort_by(F, [X|L], R):- isort_by(F, L, S), insert_by(F, X, S, R).

/**
	@form pinsert_by(F, Value1,Value2, List1,List2, NewList1,NewList2)
	@descr Given two lists and two values, insert @Value1 into @List1 as 
	follows: if position of @Value1 in @NewList1 is 'p' then position of
	@Value2 in @NewList2 is also 'p'. Note that @NewList2 may not be
	sorted at the end. The insertion is guided by function @F.
	@constrs @List1 and @List2 must have the same length.
*/
pinsert_by(_, X,Y,      [],     [],       [X],      [Y]):- !.
pinsert_by(F, X,Y, [Xx|Xs],[Yy|Ys], [X,Xx|Xs],[Y,Yy|Ys]):- call(F,X,Xx), !.
pinsert_by(F, X,Y, [Xx|Xs],[Yy|Ys],   [Xx|Xr],  [Yy|Yr]):- pinsert_by(F, X,Y, Xs,Ys, Xr,Yr).

/**
	@form pisort_by(F, List1, List2, SortedList1, OtherList2)
	@descr Sorts @List1 using function @F as guide. @List2 's elment's
	positions are modified according to the changes made in @List1.
	@constrs @List1 and @List2 must have the same length.
*/
pisort_by(_,     [],    [],  [], []):- !.
pisort_by(F, [X|Xs],[Y|Ys],  Rx, Ry):-
	pisort_by(F, Xs,Ys, Sx,Sy),
	pinsert_by(F, X,Y, Sx,Sy, Rx,Ry).

/**
	@form fuse_by(Function, List1, List2, NewList)
	@descr Fuses two sorted lists into one, guided by @Function. The
	result of fusing two lists is the concatenation of the element-wise
	sorting of the elements of the two lists: take the i-th element
	of both lists and put first the one that is defined "to go before"
	according to @Function. Assuming that Function is @<:
	<--
		fuse([X|Xs], [Y|Ys], [X,Y|R]) with X < Y
		fuse([X|Xs], [Y|Ys], [Y,X|R]) with X > Y
	-->
	where R is the fusion of Xs and Ys
*/
fuse_by(_,     [],     [],       []):- !.
fuse_by(_,     [],      L,        L):- !.
fuse_by(_,      L,     [],        L):- !.
fuse_by(F, [X|Xs], [Y|Ys], [X,Y|Ms]):- call(F, X, Y), !, fuse_by(F, Xs, Ys, Ms).
fuse_by(F, [X|Xs], [Y|Ys], [Y,X|Ms]):- call(F, Y, X), fuse_by(F, Xs, Ys, Ms).

/**
	@form pfuse_by(Function, LeftList1,RightList1, LeftList2,RightList2, NewList1,NewList2)
	@descr Fuses two pairs of sorted lists into one, guided by @Function
	and the pair of lists @LeftList1 and @LeftList2: take the i-th element
	of @LeftList1 and @LeftList2 and let ll1 and ll2 be such elements. If
	we have that <-- Function(ll1,ll2) --> is true, read as "ll1 < ll2",
	then the ordering of the second pair of lists (@RightList1 and @RightList2)
	is forced to be the same: first goes the element of @RightList1 and
	then the element of @RightList2.
	
	Assuming that @Function is '@<'
	<--
		pfuse([X1|Xs1],[Y1|Ys1], [X2|Xs2],[Y2|Ys2], [X1,X2|R1],[Y1,Y2|R2]) with X1 < X2
		pfuse([X1|Xs1],[Y1|Ys1], [X2|Xs2],[Y2|Ys2], [X2,X1|R1],[Y2,Y2|R2]) with X1 > X2
	-->
	where R1 and R2 are the result of applying
	<--
		pfuse(Xs1,Ys1, Xs2,Ys2, R1,R2)
	-->
	
	Notice that it is not necessarily true that
	<++
	!> X2 < Y2 when X1 < Y1, or that
	!> X2 > Y2 when X1 > Y1	
	++>
*/
pfuse_by(_,           [],[],           [],[],                 [],[]):- !.
pfuse_by(_,             X,Y,           [],[],                   X,Y):- !.
pfuse_by(_,           [],[],             X,Y,                   X,Y):- !.
pfuse_by(F, [X1|Xs],[Y1|Ys], [X2|Xr],[Y2|Yr], [X1,X2|Xz],[Y1,Y2|Yz]):-
	call(F, X1, X2),		% X1 goes before X2
	pfuse_by(F, Xs,Ys, Xr,Yr, Xz,Yz), !.
pfuse_by(F, [X1|Xs],[Y1|Ys], [X2|Xr],[Y2|Yr], [X2,X1|Xz],[Y2,Y1|Yz]):-
	pfuse_by(F, Xs,Ys, Xr,Yr, Xz,Yz).

/**
	@form insert(Value, List, NewList)
	@descr Applies ?insert_by/4 with operator '@<'.
*/
insert(X, L, R):- insert_by(@<, X, L, R).

/**
	@form pinsert(Value1,Value2, List1,List2, NewList1,NewList2)
	@descr Applies ?pinsert_by/7 with operator '@<'.
*/
pinsert(X,Y, Xs,Ys, SX,SY):- pinsert_by(@<, X,Y, Xs,Ys, SX,SY).

/**
	@form isort(List1, SortedList)
	@descr Applies ?isort_by/3 with operator '@<'.
*/
isort(L, R):- isort_by(@<, L, R).

/**
	@form pisort(List1,List2, SortedList1,SortedList2)
	@descr Applies ?pisort_by/5 with operator '@<'.
*/
pisort(X,Y, SX,SY):- pisort_by(@<, X,Y, SX,SY).

/**
	@form fuse(List1,List2, NewList)
	@descr Applies ?fuse_by/4 with operator '@<'.
*/
fuse(X,Y, M):- fuse_by(@<, X,Y, M).

/**
	@form pfuse(List1,List2, SortedList1,SortedList2, NewList1,NewList2)
	@descr Applies ?pfuse_by/7 with operator '@<'.
*/
pfuse(X,Y, SX,SY, RX,RY):- pfuse_by(@<, X,Y, SX,SY, RX,RY).
