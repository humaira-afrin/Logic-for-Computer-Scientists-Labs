% Predikat som givet en lista genererar en lista F med längd L som man finner konsekutivt i första listan
% F är dellista och L är längden på dellistan

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).
length1([],0).
length1([_|T],N) :- length1(T,N1), N is N1+1.

partstring(List, L, F) :-
    % SubList blir alltså List fast med olika startpunkter
    append(_,SubList, List),
    % Tar bort alla tomma listor
    SubList \= [],
    write(SubList),
    % Appenderar alla element i sublisten en för en
    % Man får alla mojliga konsekutiva substrings då SubList börjar vid olika punkter 
    append(F, _, SubList), 
    % Printar längden på F
    length1(F,L).

% Förklaring:
% Sublist= abcd
% F kan vara a, ab, abc, abcd
% Sublist= bcd
% F kan vara b, bc, bcd
% Denna monster fortsätter och man får alla delsträngar
