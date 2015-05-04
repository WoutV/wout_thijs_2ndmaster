:- lib(ic).
:- lib(lists).
:- lib(ic_global).

battleshits(Hints, Rows, Columns,Solution):-
    initialize_boats(Hints,Solution),
	sumList(Rows,Boats),
	Water is (100-Boats),
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
	check_water(OneList,Water),
	check_boats(Solution),
    check_rows(Rows,Solution),
    check_columns(Columns,Solution),
    write('searching'),nl,
	search(OneList,0,input_order,indomain,complete,[backtrack(B)]),
    pretty_print(Solution, Rows, Columns,B).
	
pretty_print(Solution, Rows, Columns,B):-
	(for(N,1,10),
    param(Solution), param(Rows)
    do
        Row is Solution[N,1..10],
		get_element(Rows,N,Tally),
        writeRow(Row),write(" "),write(Tally),nl
    ),
	(for(N,1,10),
    param(Columns)
    do
		get_element(Columns,N,Tally),
        write(Tally)
    ).
	
writeRow(List):-
(for(N,1,10),
	param(List)
	do
		get_element(List,N,Elem),
		to_string(Elem,Char),
		write(Char)
).
	
check_water(List,Amount):-
	occurrences(0,List,Amount).
    
check_boats(Solution):-
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
	%% check_length2(Solution, 3),
	%% check_length3(Solution,2),
	check_length4(Solution,1),
 %%    check_submarines(OneList),
    no_touching(Solution).


no_touching(Solution):-
	for(N,1,10),
	param(Solution)
    do
    	Row is Solution[N,1..10],
    	write('roooow'),nl,
    	check_row_touching(Row),
    	(N>1 ->
    		(Above is N-1,
    		RowAbove is Solution[Above,1..10],
    		write('above'),nl,
    		check_above(Row,RowAbove)
    		)
    		;
    		true
    	)
    	%% ),
		%% (N=\=10 ->
		%% 	(Below is N+1,
		%% 	RowBelow is Solution[Below,1..10],
		%% 	check_below(Row,RowBelow)
		%% 	)
		%% 	;
		%% 	true
	.

check_above(Row,Above):-
	for(N,1,10),
	param(Row),param(Above)
	do
		write(N),nl,
		get_element(Row,N,Elem),
		get_element(Above,N,AboveEl),
		Left is N-1,
		Right is N+1,
		get_element(Above,Left,LeftElem),
		get_element(Above,Right,RightElem),
		check_diagonal(LeftElem,Elem),
		check_diagonal(RightElem,Elem), 
		check_straight_above(AboveEl,Elem).


check_straight_above(Above,Elem):-
	write('straight'),nl,
	write(Above),nl,write(Elem),nl,
	#=(Elem,0,ElemZero),
	#=(Above,0,AboveZero),
	#\=(Above,4,AboveNotFour),
	#\=(Elem,0,ElemNotZero),

	#=(Above,4,AboveTop),
	#=(Elem,5,ElemBottom),
	#=(Elem,2,ElemMid),
	#=(Above,2,AboveMid),
	or(ElemBottom,ElemMid,ElemBotOrMid),
	and(AboveTop,ElemBotOrMid,ElementBotOrMid),
	and(AboveMid,ElemBotOrMid,AboveIsMid),
	and(ElemZero,AboveNotFour,ElementIsWater),
	and(ElemNotZero,AboveZero,AboveIsZero),


	sumOfList([ElementIsWater,AboveIsZero,ElementBotOrMid,AboveIsMid],1).


check_diagonal(E,Other) :-
	write('check_diagonal'),nl,
	#\=(E,0,ENotZero),
	#\=(Other,0,ONotZero),
	#=(E,0,EZero),
	#=(Other,0,OZero),
	and(ENotZero,OZero,B1),
	and(EZero,ONotZero,B2),
	and(EZero,OZero,BothZero),
	#\=(B1,B2,OneOfBoth),
	sumOfList([BothZero,OneOfBoth],1).

check_row_touching(List):-
	for(N,2,9),
	param(List)
	do
		get_element(List,N,Elem),
		Left is N-1,
		Right is N+1,
		get_element(List,Left,LeftElem),
		get_element(List,Right,RightElem),
		checkLeft(Elem,LeftElem),
		checkRight(Elem,RightElem).


%% Checks whether or not the elements are compatible. The given LeftElement is located on
%% the left of Element
checkLeft(Element,LeftElement):-
	#>(LeftElement,2,B1),
	#=(Element,0,B2),
	and(B1,B2,ElementIsWater),

	#<(LeftElement,3,B3),
	#\=(LeftElement,0,B4),
	and(B3,B4,ElisLorM),
	#=(Elem,3,El3),
	#=(Elem,2,El2),
	or(El3,El2,ElemMorR),
	and(ElemMorR,ElisLorM,ElementIsRightOrMid),

	#=(LeftElement,0,LeftWater),
	#\=(Elem,3,ElemNotR),
	and(LeftWater,ElemNotR,ElementNotR),
	sumOfList([ElementIsWater,ElementIsRightOrMid,ElementNotR],1).

%% Checks whether or not the elements are compatible. The given RightElement is located on
%% the right of Element
checkRight(Element,RightElement):-
	#>(RightElement,3,B1), 
	#=(RightElement,1,B2), 
	or(B1,B2,ElementShouldBeWater),
	#=(Element,0,B3),
	and(B3,ElementShouldBeWater,ElementIsWater),

	#<(RightElement,4,Smaller),
	#>(RightElement,1,Larger),
	and(Smaller,Larger,RightIsLeftOrMid),
	#=(Elem,1,El1),
	#=(Elem,2,El2),
	or(El1,El2,ElemMorL),
	and(RightIsLeftOrMid,ElemMorL,ElementIsLeftOrMid),

	#=(RightElement,0,RightWater),
	#\=(Elem,1,ElemNotL),
	and(RightWater,ElemNotL,ElementNotL),
	sumOfList([ElementIsWater,ElementIsLeftOrMid,ElementNotL],1).




% Ensures that there are only N occurences of a boat of length 2
check_length2(Solution, N):-
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
	count2(1,3,OneList,H),
	transpose(List,Trans),
	createOneList(Trans,OneList2),
	count2(4,5,OneList2,V),
	sumOfList([H,V],N).

% Ensures that there are only N occurences of a boat of length 3
check_length3(Solution, N):- 
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
	count3(1,2,3,OneList,H),
	transpose(List,Trans),
	createOneList(Trans,OneList2),
	count3(4,2,5,OneList2,V),
	sumOfList([H,V],N).

% Ensures that there are only N occurences of a boat of length 4
check_length4(Solution, N):- 
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
	count4(1,2,2,3,OneList,H),
	transpose(List,Trans),
	createOneList(Trans,OneList2),
	count4(4,2,2,5,OneList2,V),
	sumOfList([H,V],N).

count_Horizontal2(Solution,H):-
	count2(1,3,Solution,H).
	
count_Vertical2(Solution,N):-
	count2(4,5,Solution,N).

 check_submarines(Solution):-
    occurrences(6,Solution,4).
	
 check_middlepieces(Solution):-
     occurrences(2,Solution,4).

    
check_columns([],[]).
check_columns(Columns,Solution):-
    (for(N,1,10),
    param(Solution),
    param(Columns)
    do
        get_element(Columns,N,Tally),
        Column is Solution[1..10,N],
        check_row(Tally,Column)
    ).
    
check_rows([],[]).
check_rows(Rows,Solution):-
    (for(N,1,10),
    param(Solution),
    param(Rows)
    do
        get_element(Rows,N,Tally),
        Row is Solution[N,1..10],
        check_row(Tally,Row)
    ).
    
get_element(List,N,Elem):-
    get_element(N,List,0,Elem).
    
get_element(_,[],_,_).
get_element(N,[Elem|Others],Current,Result):-
    Next is Current+1,
    (Next==N ->
        Result = Elem
    ;
        get_element(N,Others,Next,Result)
    ).
	
check_row(Tally,SolutionRow):-
	N is (10-Tally),
	occurrences(0,SolutionRow,N).
    
initialize_boats(Hints, Solution):-
    dim(Solution,[10,10]),
    Solution::0..6,
    
    (foreach(Hint,Hints),
        param(Solution)
    do
        set_hint(Hint,Solution)
    ).

set_hint((Row,Column,Boat),Solution):-
    to_string(Int,Boat),
    subscript(Solution,[Row,Column],Int).

%Takes a list of lists and returns the concatenation of the lists into one list.                
createOneList(L,R):-createOneList(L,[],R).
createOneList([],Acc,Acc).
createOneList([H|T],Acc,List):-
    append(Acc,H,R1),
    createOneList(T,R1,List).
	
count2(_,_,[_],0).
count2(A,B,[X,Y|T],N):- 
	count2(A,B,[Y|T],N1), 
	#=(A,X,B1), 
	#=(B,Y,B2), 
	and(B1,B2,B3),
	N#=B3+N1.


count3(_,_,_,[_,_],0).
count3(A,B,C,[X,Y,Z|T],N):- 
	count3(A,B,C,[Y,Z|T],N1), 
	#=(A,X,B1), 
	#=(B,Y,B2), 
	and(B1,B2,B3),
	#=(C,Z,B4),
	and(B3,B4,Btotal),
	N#=Btotal+N1.

count4(_,_,_,_,[_,_,_],0).
count4(A,B,C,D,[W,X,Y,Z|T],N):- 
	count4(A,B,C,D,[X,Y,Z|T],N1), 
	#=(A,W,B1), 
	#=(B,X,B2), 
	and(B1,B2,B3),
	#=(C,Y,B4),
	and(B3,B4,B34),
	#=(D,Z,B5),	
	and(B34,B5,Btotal),
	N#=Btotal+N1.


sumList([], 0).
sumList([H|T], Sum) :-
   sum(T, Rest),
   Sum is H + Rest.

% Transpose matrix - http://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog   
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


%% Calculates the sum of all variables in list
sumOfList(List,Sum):-
    (
	foreach(X,List),
   fromto(Expr,S1,S2,0)
   do
   S1 = X + S2
   ),
   Sum $= eval(Expr).


to_string(0,".").
to_string(1,"l").
to_string(2,"m").
to_string(3,"r").
to_string(4,"t").
to_string(5,"b").
to_string(6,"c").
to_string(0,water).
to_string(1,left).
to_string(2,middle).
to_string(3,right).
to_string(4,top).
to_string(5,bottom).
to_string(6,circle).
