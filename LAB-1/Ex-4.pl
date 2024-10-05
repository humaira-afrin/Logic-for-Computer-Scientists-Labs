granne(a,b).
granne(b,c).
granne(c,d).
granne(d,e).
granne(e,f).
granne(f,a).
granne(c,g).
granne(g,i).

granne1(A,X):- granne(A,X); granne(X,A).

path1(A, A, _, [A]).
path1(A, B, Acc, [A|T]) :- granne1(A, X),\+ memberchk(X, Acc),         
    path1(X, B, [X|Acc], T).     

path(A,B,T):- path1(A,B,[A],T).
