:- lib(ic).

solve(Puzzle):- rowConstraint(Puzzle),
                columnConstraint(Puzzle),
                blockConstraint(Puzzle),
                write(Puzzle).
                
rowConstraint(Puzzle):-
    (foreach(Row,Puzzle)
     do
        Row::1..9,
        alldifferent(Row) 
    ).
    
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

% Constraints for all blocks - http://programmablelife.blogspot.co.at/2012/07/adventures-in-declarative-programming.html
blockConstraint([A,B,C,D,E,F,G,H,I]):-
    blocks(A,B,C),
    blocks(D,E,F),
    blocks(G,H,I).
    
blocks([], [], []).      
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
  [A,B,C,D,E,F,G,H,I]::1..9,
  alldifferent([A,B,C,D,E,F,G,H,I]),     
  blocks(Bs1, Bs2, Bs3).