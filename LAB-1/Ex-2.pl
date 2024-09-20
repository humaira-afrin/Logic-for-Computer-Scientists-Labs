% Definition of a list
list([]).
list([H|T]) :- list(T).      

% main 
remove_duplicates(Input, Out):- remove_duplicates1(Input,[],Out).

% Base 
remove_duplicates1([],_, []). 

%  i_f head is in X, skip it and move forward with the tail of the list.
remove_duplicates1([H|T],X,Out):- memberchk(H,X), remove_duplicates1(T,X,Out). 

% Otherwise, head not repeated, add it to X
remove_duplicates1([H|T],X,[H|Out]):- \+ memberchk(H,X) , remove_duplicates1(T,[H|X],Out). 