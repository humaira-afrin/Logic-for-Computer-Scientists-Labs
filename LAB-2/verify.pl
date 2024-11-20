:- discontiguous helper/4.

verify(InputFileName) :- 
    see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof).

valid_proof(Prems, Goal, Proof) :- 
    helper(Prems, Goal, Proof, []),
    last(Proof, [_, Goal, _]), !.

helper(Prems, Goal, [Del | RestSteps], Bevisat) :-
    Del = [FirstLine | RestOfSection],
    FirstLine = [_, _, assumption],
    % Box handling starts here
    (RestOfSection = [] ; 
     append([FirstLine], Bevisat, TempBevisat),
     helper(Prems, _, RestOfSection, TempBevisat)), % Box ends here
    append([Del], Bevisat, UpdatedBevisat),
    helper(Prems, Goal, RestSteps, UpdatedBevisat).

% Helper rules without singleton variables, using just Bevisat and relevant data
helper(Prems, Goal, [[Num, Step, Rule] | Rest], Bevisat) :- 
    (premise_rule(Prems, Rule, Bevisat, Step) ;
     andint(Prems, Rule, Bevisat, Step) ;
     andel1(Prems, Rule, Bevisat, Step) ;
     andel2(Prems, Rule, Bevisat, Step) ;
     negel(Prems, Rule, Bevisat, Step) ;
     negint(Prems, Rule, Bevisat, Step) ;
     contel(Prems, Rule, Bevisat, Step) ;
     neg_neg_int(Prems, Rule, Bevisat, Step) ;
     neg_neg_el(Prems, Rule, Bevisat, Step) ;
     copy(Prems, Rule, Bevisat, Step) ;
     mt(Prems, Rule, Bevisat, Step) ;
     pbc(Prems, Rule, Bevisat, Step) ;
     lem(Prems, Rule, Bevisat, Step) ;
     impint(Prems, Rule, Bevisat, Step) ;
     impel(Prems, Rule, Bevisat, Step) ;
     orint1(Prems, Rule, Bevisat, Step) ;
     orint2(Prems, Rule, Bevisat, Step) ;
     orel(Prems, Rule, Bevisat, Step)),
    append([[Num, Step, Rule]], Bevisat, UpdatedBevisat),
    helper(Prems, Goal, Rest, UpdatedBevisat).

helper(Prems, Goal, [[_, Goal, Rule]], Bevisat):-  
    (premise_rule(Prems, Rule, Bevisat, Goal) ;
     andint(Prems, Rule, Bevisat, Goal) ;
     andel1(Prems, Rule, Bevisat, Goal) ;
     andel2(Prems, Rule, Bevisat, Goal) ;
     negel(Prems, Rule, Bevisat, Goal) ;
     negint(Prems, Rule, Bevisat, Goal) ;
     contel(Prems, Rule, Bevisat, Goal) ;
     neg_neg_int(Prems, Rule, Bevisat, Goal) ;
     neg_neg_el(Prems, Rule, Bevisat, Goal) ;
     copy(Prems, Rule, Bevisat, Goal) ;
     mt(Prems, Rule, Bevisat, Goal) ;
     pbc(Prems, Rule, Bevisat, Goal) ;
     lem(Prems, Rule, Bevisat, Goal) ;
     impint(Prems, Rule, Bevisat, Goal) ;
     impel(Prems, Rule, Bevisat, Goal) ;
     orint1(Prems, Rule, Bevisat, Goal) ;
     orint2(Prems, Rule, Bevisat, Goal) ;
     orel(Prems, Rule, Bevisat, Goal)).

% Handling the assumption box
helper(Prems, _, [Del | Rest], Bevisat) :-
    Del = [FirstLine | T],
    FirstLine = [_, _, assumption],
    % Box  starts here
    (T = [] ; 
     append([FirstLine], Bevisat, TempBevisat),
     helper(Prems, _, T, TempBevisat)), % Box ends here
    append([Del], Bevisat, UpdatedBevisat),
    helper(Prems, _, Rest, UpdatedBevisat).

% Box predicate without Prems and Goal
box(First, Last, Bevisat) :-
    member([First|T], Bevisat),
    last(T, Last).

box(Line, Line, Bevisat) :-
    member([Line], Bevisat).

% Now we define the specific rules directly in helper predicates:

% Premise
premise_rule(Prems, premise, Bevisat, A) :- 
    member(A, Prems).

% ∧i
andint(_, andint(X, Y), Bevisat, and(A, B)) :- 
    member([X, A, _], Bevisat),
    member([Y, B, _], Bevisat).

% ∧e 1
andel1(_, andel1(X),Bevisat, A) :- 
    member([X, and(A, _), _], Bevisat).

% ∧e 2
andel2(Prems, andel2(X), Bevisat, A) :- 
    member([X, and(_, A), _], Bevisat).

% ¬e (A,¬A |- ⊥) Motsägelse
negel(Prems, negel(X, Y), Bevisat, motsagelse) :- 
    member([X, A, _], Bevisat),
    member([Y, neg(A), _], Bevisat).

% ¬i (A (assumption),....⊥ |- ¬A)
negint(Prems, negint(X, Y), Bevisat, neg(A)) :- 
    box([X, A, assumption], [Y, motsagelse, _], Bevisat).

% ⊥e (⊥ allows anything)
contel(Prems, motsagelseel(X), Bevisat, _) :- 
    member([X, motsagelse, _], Bevisat).


% ¬¬i
neg_neg_int(Prems, negnegint(X), Bevisat, neg(neg(A))) :- 
    member([X, A, _], Bevisat).

% ¬¬e (¬¬A = A)
neg_neg_el(Prems, negnegel(X), Bevisat, A) :- 
    member([X, neg(neg(A)), _], Bevisat).

% Copy
copy(Prems, copy(X), Bevisat, A) :- 
    member([X, A, _], Bevisat).

% MT (A-->B, not B |- not A)
mt(Prems, mt(X, Y), Bevisat, neg(A)) :- 
    member([X, imp(A, B), _], Bevisat),
    member([Y, neg(B), _], Bevisat).

% PBC (Assume not A....⊥ |- A)
pbc(Prems, pbc(X, Y), Bevisat, A) :- 
    box([X, neg(A), assumption], [Y, motsagelse, _], Bevisat).

% LEM
lem(Prems, lem, Bevisat, or(A, neg(A))).

% -->i (A (assumption),......,B |- A-->B)
impint(Prems, impint(X, Y), Bevisat, imp(A, B)) :- 
    box([X, A, assumption], [Y, B, _], Bevisat).

% -->e  (A, A-->B |- B)
impel(Prems, impel(X, Y), Bevisat, B) :- 
    member([X, A, _], Bevisat),
    member([Y, imp(A, B), _], Bevisat).

% ∨i 1
orint1(Prems, orint1(X), Bevisat, or(A, _)) :- 
    member([X, A, _], Bevisat).

% ∨i 2
orint2(Prems, orint2(X), Bevisat, or(_, A)) :- 
    member([X, A, _], Bevisat).

% ∨e assumption B leads to A and assumption C leads to A
orel(Prems, orel(I, II, III, IV, V), Bevisat, A) :- 
    member([I, or(B, C), _], Bevisat),
    box([II, B, assumption], [III, A, _], Bevisat),
    box([IV, C, assumption], [V, A, _], Bevisat).
