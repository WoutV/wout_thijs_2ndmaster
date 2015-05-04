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








:- lib(ic).
:- lib(lists).

/*
solve(Puzzle):-
    T = Puzzle,
    solve(Puzzle,naive),
    solve(T,first_fail),
    solve(Puzzle,middle_out),
    solve(Puzzle,moff),
    solve(Puzzle,moffmo).
*/

solve(P,Search):- 
                transpose(P,Puzzle),
                rowConstraint(Puzzle),
                columnConstraint(Puzzle),
                blockConstraint(Puzzle),
                createOneList(Puzzle,R),
                search(Search,R,B),
                pretty_print(Puzzle,B,Search).

%Takes a list of lists and returns the concatenation of the lists into one list.                
createOneList(L,R):-createOneList(L,[],R).
createOneList([],Acc,Acc).
createOneList([H|T],Acc,List):-
    append(Acc,H,R1),
    createOneList(T,R1,List).

%Each list in the list of lists contains all number from 1 to 9.
rowConstraint(Puzzle):-
    (foreach(Row,Puzzle)
     do
        Row::1..9,
        alldifferent(Row) 
    ).
    
/* Pretty Print L */
pretty_print(List,B,Search) :-
    (foreach(Row,List)
    do 
        write(Row),nl
	),
    write(search - Search),nl,    
    write(backtracks-B),nl,
    write(-----------------------),nl.
    
/* SEARCH */
search(naive,List,B) :-
    search(List,0,input_order,indomain,complete,[backtrack(B)]).
    
search(middle_out,List,B) :-
    middle_out(List,MOList),
    search(MOList,0,input_order,indomain,complete, [backtrack(B)]).
    
search(first_fail,List,B) :-
    search(List,0,first_fail,indomain,complete, [backtrack(B)]).
    
search(moff,List,B) :-
    middle_out(List,MOList),
    search(MOList,0,first_fail,indomain,complete, [backtrack(B)]).
    
search(moffmo,List,B) :-
    middle_out(List,MOList),
    search(MOList,0,first_fail,
    indomain_middle,complete, [backtrack(B)]).    
    
        
% Each column contains all numbers between 1 and 9.    
columnConstraint(Puzzle):-
     transpose(Puzzle, Columns),
     rowConstraint(Columns).

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
        
% TODO? scalet niet
% Constraints for all blocks - http://programmablelife.blogspot.co.at/2012/07/adventures-in-declarative-programming.html
blockConstraint([]).
blockConstraint([A,B,C|Rest]):-
    blocks(A,B,C),
    blockConstraint(Rest).
    
blocks([], [], []).      
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
  [A,B,C,D,E,F,G,H,I]::1..9,
  alldifferent([A,B,C,D,E,F,G,H,I]),     
  blocks(Bs1, Bs2, Bs3).
  
  
%OTHER VIEWPOINT
solve_2(Puzzle,Search,Numbers):-
    create_numbers(Numbers),
    createOneList(Puzzle,List),
    List::1..9,
    fill_in_numbers(Puzzle,Numbers),    
    is_in_each_block(Numbers),
    search(naive,List,B).        
        
% Initializes an array representing each number from 1 to 9 and its positions in the sudoku puzzle.
create_numbers(Numbers):-
    dim(Numbers,[9,2]),
    (for(N,1,9),
    param(Numbers)
    do
        dim(Positions,[9,1]),
        (for(I,1,9),
        param(Positions)
        do
            A::1..9, 
            subscript(Positions,[I,1],A)
        ),
        Row is Positions[1..9,1],
        alldifferent(Row),
    subscript(Numbers,[N,1],N),
    subscript(Numbers,[N,2],Positions)
    ).

 fill_in_numbers(Puzzle,Numbers):-
    (for(Row,1,9),
    param(Numbers),
    param(Puzzle)
    do
        (for(Column,1,9),
        param(Row),
        param(Numbers),
        param(Puzzle)
        do  
            get_sudoku_element(Puzzle,Row,Column,Element),
            (\+(var(Element)) ->
                            Positions is Numbers[Element,2],
                            subscript(Positions,[Row,1],Column),
                            subscript(Numbers,[Element,2],Positions)           
                            ;
                            true)
        )                   
    ).          

is_in_each_block(Numbers):-
    (for(N,1,9), %Elk getal afgaan
    param(Numbers)
    do
        Number is Numbers[N,2],
        (for(BlockRow,1,3), %Elk rij van blokken
        param(Number),
        param(Numbers)
            do
                Start is 1+((BlockRow-1)*3),
                Stop is BlockRow*3,
                RelevantRows is Number[Start..Stop,1],
                sort_rows(RelevantRows,Sorted),
                check_correct(Sorted)
         )
    ).
    
check_correct([A,B,C]):-
        A#>=1,
        A#=<3,
        B#>=4,
        B#=<6,
        C#>=7,
        C#=<9.
                
sort_rows([A,B,C],[A,B,C]):-
    >=(C,B,R1),
    >=(B,A,R2),
    R1+R2#>1.
        
sort_rows([A,B,C],[A,C,B]):-
    >=(B,C,R1),
    >=(C,A,R2),
    R1+R2#>1.
    
sort_rows([A,B,C],[B,C,A]):-
    >=(A,C,R1),
    >=(C,B,R2),
    R1+R2#>1.
    
sort_rows([A,B,C],[B,A,C]):-
    >=(C,A,R1),
    >=(A,B,R2),
    R1+R2#>1.
    
sort_rows([A,B,C],[C,A,B]):-
    >=(B,A,R1),
    >=(A,C,R2),
    R1+R2#>1.
    
sort_rows([A,B,C],[C,B,A]):-
    >=(A,B,R1),
    >=(B,C,R2),
    R1+R2#>1.
                            
 %Retrieves the element at given Row and given Column.
 get_sudoku_element(Puzzle,Row,Column,Element):-get_sudoku_element(Puzzle,1,Row,Column,Element).
 
 get_sudoku_element([_|RestOfPuzzle],CurrentRow,Row,Column,Element):-
    CurrentRow<Row,
    NewRow is CurrentRow+1,
    get_sudoku_element(RestOfPuzzle,NewRow,Row,Column,Element).
    
 get_sudoku_element([PuzzleRow|_],CurrentRow,Row,Column,Element):-
    CurrentRow=:=Row,
    get_element(PuzzleRow,Column,Element).
    
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
