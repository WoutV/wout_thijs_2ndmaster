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
solve_2(Puzzle,Search,Blocks):-
    to_blocks(Puzzle,Blocks),
    blockConstraint(Blocks),
    block_columnConstraint(Blocks).
 
to_blocks([A,B,C,D,E,F,G,H,I],FinalBlocks):-
 to_blocks(A,B,C, [], Blocks1),
 to_blocks(D,E,F, [], Blocks2),
 to_blocks(G,H,I, [], Blocks3),
 Blocks = [Blocks1,Blocks2,Blocks3],
 createOneList(Blocks,FinalBlocks).
  
to_blocks([], [], [],Acc,Acc).      
to_blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3], Acc, Blocks) :-
  [A,B,C,D,E,F,G,H,I]::1..9,
  alldifferent([A,B,C,D,E,F,G,H,I]),
  append(Acc,[[A,B,C,D,E,F,G,H,I]],Acc2),
  to_blocks(Bs1, Bs2, Bs3,Acc2,Blocks).
      
block_columnConstraint(Blocks):-
    transpose(Blocks,Tblocks),
    blockConstraint(Tblocks).

  
