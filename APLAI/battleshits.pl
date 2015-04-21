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
    pretty_print(Solution).
	
pretty_print(Solution):-
	(for(N,1,10),
    param(Solution)
    do
        Row is Solution[N,1..10],
        writeRow(Row),nl
    ).
	
writeRow(List):-
(for(N,1,10),
	param(List)
	do
		get_element(Row,N,Elem),
		write(Elem)
).
	
check_water(List,Amount):-
	count(0,List,Amount).
    
check_boats(Solution):-
	List is Solution[1..10,1..10],
	createOneList(List,OneList),
    check_submarines(OneList),
	check_middlepieces(OneList).

 check_submarines(Solution):-
    count(6,Solution,4).


 check_middlepieces(Solution):-
     count(4,Solution,4).

    
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

sumList([], 0).
sumList([H|T], Sum) :-
   sum(T, Rest),
   Sum is H + Rest.


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
