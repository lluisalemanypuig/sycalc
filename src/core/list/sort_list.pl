
/***
	@descr This file contains a number of simple sorting algorithms,
	and two "list fusion" algorithms.
*/

/**
	@form insert_by(Function, Value, List, NewList)
	@descr Inserts Value into List according to Function using the
	insertion sort algorithm.
*/
insert_by(_, X, [], [X]):- !.
insert_by(F, X, [Y|L], [X,Y|L]):- call(F, X, Y), !.   % X < Y
insert_by(F, X, [Y|L], [Y|R]):- insert_by(F, X, L, R).

/**
	@form isort_by(Function, List, SortedList)
	@descr Insertion sort according guided by Function.
*/
isort_by(_, [], []):- !.
isort_by(F, [X|L], R):- isort_by(F, L, S), insert_by(F, X, S, R).

/**
	@form pinsert_by(F, Value1,Value2, List1,List2, NewList1,NewList2)
	@constraints List1 and List2 must have the same length.
	@descr Given two lists and two values, insert a into A as follows:
	if position of Value1 in NewList1 is 'p' then position of Value2
	in NewList2 is also 'p'. Note that NewList2 may not be sorted at the
	end. The insertion is guided by function F.
*/
pinsert_by(_, X,Y,      [],     [],       [X],      [Y]):- !.
pinsert_by(F, X,Y, [Xx|Xs],[Yy|Ys], [X,Xx|Xs],[Y,Yy|Ys]):- call(F,X,Xx), !.
pinsert_by(F, X,Y, [Xx|Xs],[Yy|Ys],   [Xx|Xr],  [Yy|Yr]):- pinsert_by(F, X,Y, Xs,Ys, Xr,Yr).

/**
	@form pisort_by(Function, List1, List2, SortedList1, OtherList2)
	@constraints List1 and List2 must have the same length.
	@descr Sorts List1 using function F as guide. List2's elment's
	positions are modified according to the changes made in List1.
*/
pisort_by(_,     [],    [],  [], []):- !.
pisort_by(F, [X|Xs],[Y|Ys],  Rx, Ry):-
	pisort_by(F, Xs,Ys, Sx,Sy),
	pinsert_by(F, X,Y, Sx,Sy, Rx,Ry).

/**
	@form fuse_by(Function, List1, List2, NewList)
	@constraints List1 and List2 must be sorted according to Function.
	@descr Fuses two sorted lists into one, guided by Function. The
	result of fusing two lists is the concatenation of the element-wise
	sorting of the elements of the two lists: take the i-th element
	of both lists and put first the one that is defined "to go before"
	according to Function:
		fuse([x|xs], [y|ys], [x,y|r]) with x < y
		fuse([x|xs], [y|ys], [y,x|r]) with x > y
	where r is the fusion of xs and ys
*/
fuse_by(_,     [],     [],       []):- !.
fuse_by(_,     [],      L,        L):- !.
fuse_by(_,      L,     [],        L):- !.
fuse_by(F, [X|Xs], [Y|Ys], [X,Y|Ms]):- call(F, X, Y), !, fuse_by(F, Xs, Ys, Ms).
fuse_by(F, [X|Xs], [Y|Ys], [Y,X|Ms]):- call(F, Y, X), fuse_by(F, Xs, Ys, Ms).

/**
	@form fuse_by(Function, LeftList1,RightList1, LeftList2,RightList2, NewList1,NewList2)
	@constraints LeftList1 and RightList1 must have the same length and
	be sorted, according to Function.
	@descr Fuses two pairs of sorted lists into one, guided by Function
	and the pair of lists LeftList1 and LeftList2: take the i-th element
	of LeftList1 and LeftList2. Let ll1 and ll2 be such elements. If
	ll1 goes before in the ordering and then goes ll2, then the ordering
	of the second pair of lists (RightList1 and RIghtList2) is forced
	to be the same: first goes the element of RightList1 and then the
	element of RightList2.
	
		fuse([x1|xs1],[y1|ys1], [x2|xs2],[y2|ys2], [x1,y1|r1],[x2,y2|r2]) with x1 < y1
		fuse([x1|xs1],[y1|ys1], [x2|xs2],[y2|ys2], [y1,x1|r1],[y2,x2|r2]) with x1 < y1
	
	where r1 is the fusion of xs1 and ys1 and r2 the fusion of xs2 and ys2.
	Notice that it is not necessarily true that
	-> x2 < y2 when x1 < y1, or that
	-> x2 > y2 when x1 > y1 
	
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
	@descr Applies insert_by with operator @<.
*/
insert(X, L, R):- insert_by(@<, X, L, R).

/**
	@form pinsert(Value1,Value2, List1,List2, NewList1,NewList2)
	@descr Applies pinsert_by with operator @<.
*/
pinsert(X,Y, Xs,Ys, SX,SY):- pinsert_by(@<, X,Y, Xs,Ys, SX,SY).

/**
	@form isort(List1, SortedList)
	@descr Applies isort_by with operator @<.
*/
isort(L, R):- isort_by(@<, L, R).

/**
	@form pisort(List1,List2, SortedList1,SortedList2)
	@descr Applies pisort_by with operator @<.
*/
pisort(X,Y, SX,SY):- pisort_by(@<, X,Y, SX,SY).

/**
	@form fuse(List1,List2, NewList)
	@descr Applies fuse_by with operator @<.
*/
fuse(X,Y, M):- fuse_by(@<, X,Y, M).

/**
	@form pfuse(List1,List2, SortedList1,SortedList2, NewList1,NewList2)
	@descr Applies fuse_by with operator @<.
*/
pfuse(X,Y, SX,SY, RX,RY):- pfuse_by(@<, X,Y, SX,SY, RX,RY).
