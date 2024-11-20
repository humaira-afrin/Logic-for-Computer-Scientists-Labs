
verify(InputFileName) :- 
    see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof).

valid_proof(Prems, Goal, Proof) :- 
    checker(Prems, Goal, Proof, []).


% assumption box
 checker(Prems, _, [[H | T] | Rest], Proven) :-
    H = [_, _, assumption],  
    (T = [] ; append([H], Proven, TempProven), checker(Prems, _, T, TempProven)  
    ),
    append([[H | T] | Rest], Proven, UpdatedProven), 
    checker(Prems, _, Rest, UpdatedProven).  
  
% checker calls the predicate for the rules and add it to the a new accumulator and recursively continues with the rest of the steps
checker(Prems, Goal, [[Num, Step, Rule] | Rest], Proven) :- 
    (premise_rule(Prems, Rule, Proven, Step) ; andint(Prems, Rule, Proven, Step) ; andel1(Prems, Rule, Proven, Step) ;
    andel2(Prems, Rule, Proven, Step) ; negel(Prems, Rule, Proven, Step) ; negint(Prems, Rule, Proven, Step) ;
    contel(Prems, Rule, Proven, Step) ; neg_neg_int(Prems, Rule, Proven, Step) ; neg_neg_el(Prems, Rule, Proven, Step) ;
    copy(Prems, Rule, Proven, Step) ; mt(Prems, Rule, Proven, Step) ; pbc(Prems, Rule, Proven, Step) ;
     lem(Prems, Rule, Proven, Step) ; impint(Prems, Rule, Proven, Step) ;  impel(Prems, Rule, Proven, Step) ;
     orint1(Prems, Rule, Proven, Step) ; orint2(Prems, Rule, Proven, Step) ;orel(Prems, Rule, Proven, Step)),
    append([[Num, Step, Rule]], Proven, UpdatedProven),
    checker(Prems, Goal, Rest, UpdatedProven).

% this is the last line of the steps, only checks the rules 
checker(Prems, Goal, [[_, Goal, Rule]], Proven):-  
    (premise_rule(Prems, Rule, Proven, Goal) ;
     andint(_, Rule, Proven, Goal) ;
     andel1(_, Rule, Proven, Goal) ;
     andel2(_, Rule, Proven, Goal) ;
     negel(_, Rule, Proven, Goal) ;
     negint(_, Rule, Proven, Goal) ;
     contel(_, Rule, Proven, Goal) ;
     neg_neg_int(_, Rule, Proven, Goal) ;
     neg_neg_el(_, Rule, Proven, Goal) ;
     copy(_, Rule, Proven, Goal) ;
     mt(_, Rule, Proven, Goal) ;
     pbc(_, Rule, Proven, Goal) ;
     lem(_, Rule, _, Goal) ;
     impint(_, Rule, Proven, Goal) ;
     impel(_, Rule, Proven, Goal) ;
     orint1(_, Rule, Proven, Goal) ;
     orint2(_, Rule, Proven, Goal) ;
     orel(_, Rule, Proven, Goal)).



% Box checks if starts and ends with the given paramenters and if the box only contains one line
box(First, Last, Proven) :-
    (member([First|T], Proven), last(T, Last));
    (First = Last, member([First], Proven)).

% RULES

% Premise
premise_rule(Prems, premise, _, A) :- 
    member(A, Prems).

% ∧i
andint(_, andint(X, Y), Proven, and(A, B)) :- 
    member([X, A, _], Proven),
    member([Y, B, _], Proven).

% ∧e 1
andel1(_, andel1(X),Proven, A) :- 
    member([X, and(A, _), _], Proven).

% ∧e 2
andel2(_, andel2(X), Proven, A) :- 
    member([X, and(_, A), _], Proven).

% ¬e (A,¬A |- ⊥) Motsägelse
negel(_, negel(X, Y), Proven, motsagelse) :- 
    member([X, A, _], Proven),
    member([Y, neg(A), _], Proven).

% ¬i (A (assumption),....⊥ |- ¬A)
negint(_, negint(X, Y), Proven, neg(A)) :- 
    box([X, A, assumption], [Y, motsagelse, _], Proven).

% ⊥e (⊥ allows anything)
contel(_, motsagelseel(X), Proven, _) :- 
    member([X, motsagelse, _], Proven).


% ¬¬i
neg_neg_int(_, negnegint(X), Proven, neg(neg(A))) :- 
    member([X, A, _], Proven).

% ¬¬e (¬¬A = A)
neg_neg_el(_, negnegel(X), Proven, A) :- 
    member([X, neg(neg(A)), _], Proven).

% Copy
copy(_, copy(X), Proven, A) :- 
    member([X, A, _], Proven).

% MT (A-->B, not B |- not A)
mt(_, mt(X, Y), Proven, neg(A)) :- 
    member([X, imp(A, B), _], Proven),
    member([Y, neg(B), _], Proven).

% PBC (Assume not A....⊥ |- A)
pbc(_, pbc(X, Y), Proven, A) :- 
    box([X, neg(A), assumption], [Y, motsagelse, _], Proven).

% LEM
lem(_, lem, _, or(A, neg(A))).

% -->i (A (assumption),......,B |- A-->B)
impint(_, impint(X, Y), Proven, imp(A, B)) :- 
    box([X, A, assumption], [Y, B, _], Proven).

% -->e  (A, A-->B |- B)
impel(_, impel(X, Y), Proven, B) :- 
    member([X, A, _], Proven),
    member([Y, imp(A, B), _], Proven).

% ∨i 1
orint1(_, orint1(X), Proven, or(A, _)) :- 
    member([X, A, _], Proven).

% ∨i 2
orint2(_, orint2(X), Proven, or(_, A)) :- 
    member([X, A, _], Proven).

% ∨e assumption B leads to A and assumption C leads to A
orel(_, orel(I, II, III, IV, V), Proven, A) :- 
    member([I, or(B, C), _], Proven),
    box([II, B, assumption], [III, A, _], Proven),
    box([IV, C, assumption], [V, A, _], Proven).
