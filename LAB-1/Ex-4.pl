granne(a,b).
granne(b,c).
granne(c,d).
granne(d,e).
granne(e,f).
granne(f,a).
granne(c,g).
granne(g,i).


granne1(A):-(granne(A,X);
granne(X,A)), write(X).
