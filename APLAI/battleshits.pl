:- lib(ic).
:- lib(lists).

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
	search(OneList,0,input_order,indomain,complete,[backtrack(B)]),
    pretty_print(Solution, Rows, Columns).
	
pretty_print(Solution, Rows, Columns):-
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
	count(0,List,Amount).
    
check_boats(Solution):-
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
    check_submarines(OneList),
	check_middlepieces(OneList),
	%check_ends(OneList),
	check_length2(Solution, 3).
	
check_length2(Solution, N):-
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
	count2([1,3],Solution,H),
	transpose(List,Trans),
	createOneList(Trans,OneList2),
	count2([4,5],OneList2,V),
	N #= H+V.
	
check_ends(Solution):-
	count(1,Solution,H),
	count(3,Solution,H),
	count(4,Solution,V),
	count(5,Solution,V).

	
count_Horizontal2(Solution,H):-
	count2(1,3,Solution,H).
	
count_Vertical2(Solution,N):-
	count2(4,5,Solution,N).

 check_submarines(Solution):-
    count(6,Solution,4).
	
 check_middlepieces(Solution):-
     count(2,Solution,4).

    
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
	count(0,SolutionRow,N).
    
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
	
	
count(X,[],0).
count(X,[H|T],N):- count(X,T,N1), #=(X,H,B), N#=B+N1.

count2(_,_,[],0).
count2(A,B,[X,Y|T],N):- count2(A,B,T,N1), #=(A,X,B1), #=(B,Y,B2), and(B1,B2,B3),N#=B3+N1.

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
