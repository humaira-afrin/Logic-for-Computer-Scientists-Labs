select(X,[X|T],T).
select(X,[Y|T],[Y|R]) :- select(X,T,R).
memberchk(X,L) :- select(X,L,_), !.  

% main predikat (arity 2)
remove_duplicates(Input, Out):- remove_duplicates1(Input,[],Out).

% Base 
remove_duplicates1([],_, []). 

%  if head is in X, skip it and move forward with the tail of the list.
remove_duplicates1([H|T],X,Out):- memberchk(H,X), remove_duplicates1(T,X,Out). 

% Otherwise, head not repeated, add it to X and to the outpul list
remove_duplicates1([H|T],X,[H|Out]):- \+ memberchk(H,X) , remove_duplicates1(T,[H|X],Out). 
