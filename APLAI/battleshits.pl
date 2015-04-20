:- lib(ic).
:- lib(lists).

battleshits(Hints, Rows, Columns,Solution):-
    initialize_boats(Hints,Solution),
    check_rows(Rows,Solution),
    check_columns(Columns,Solution),
    check_boats(Solution).
    %pretty_print(Solution).
    
 check_boats(Solution):-
    check_submarines(Solution).
    
 check_submarines(Solution):-
    (foreachelem(Elem,Solution),
    fromto(0,In,Out,Sum)
    do (
       Element#=6 -> Out is In + 1
       ),
    Sum#=4).
    
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
    (foreach(Element,SolutionRow),
    fromto(0,In,Out,Sum)
    do (
       Element#>0 -> Out is In + 1
       ),
    Sum#=Tally).
    
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