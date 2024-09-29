granne(a,b).
granne(b,c).
granne(c,d).
granne(d,e).
granne(e,f).
granne(f,a).
granne(c,g).
granne(g,i).

granne1(A,X):- granne(A,X); granne(X,A).

path(A, A, _, [A]).
path(A, B, Acc, [A|T]) :- granne1(A, X),\+ memberchk(X, Acc),         
    path(X, B, [X|Acc], T).     
