
% Definition of a list
list([]).
list([H|T]) :- list(T).       

% Base
remove_duplicates([], []). 

% If the Head is repeated
remove_duplicates([H|T],Out):- memberchk(H,T), remove_duplicates(T,Out). 

% Otherwise
remove_duplicates([H|T], [H|Out]):- \+ memberchk(H,T) , remove_duplicates(T,Out). 