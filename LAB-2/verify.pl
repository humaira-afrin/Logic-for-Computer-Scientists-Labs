% Från instruktion
verify(InputFileName) :- see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof).


% R E G L E R
% X och Y är step numbers och predikate följer tsrukturen 
% rule(Regel, Premises, Bevisat, conclusion)

%  Premise
rule(premise, Premis, _, A) :- member(A, Premis).

% ∧i 
rule(andint(X, Y), _, Bevisat, and(A, B)) :- 
    member([X, A, _], Bevisat),
    member([Y, B, _], Bevisat).

% ∧e 1
rule(andel1(X), _, Bevisat, A) :- 
    member([X, and(A, _), _], Bevisat).

% ∧e 2
rule(andel2(X), _, Bevisat, A) :- 
    member([X, and(_, A), _], Bevisat).

% ¬e (A,¬A |- ⊥) Motsägelse
rule(negel(X, Y), _, Bevisat, motsagelse) :- 
    member([X, A, _], Bevisat),
    member([Y, neg(A), _], Bevisat).

% Box kollar om Bevisat har en en som börjar med X och slutar med Y
box(X,Y,Bevisat):- member(Z,Bevisat),
     append([X],_,Z), append(_,[Y],Z).

% ¬i (A (assumption),....⊥ |- ¬A)
rule(negint(X, Y), _, Bevisat, neg(A)) :- 
    box([X, A, assumption], [Y, motsagelse, _], Bevisat).

% ⊥e (⊥ då kan man skriva vadsomhelst)
rule(motsagelseel(X), _, Bevisat, _) :- 
    member([X, motsagelse, _], Bevisat).

% ¬¬i
rule(negnegint(X), _, Bevisat, neg(neg(A))) :- 
    member([X, A, _], Bevisat).

% ¬¬e (¬¬A = A)
rule(negnegel(X), _, Bevisat, A) :- 
    member([X, neg(neg(A)), _], Bevisat).

% Copy
rule(copy(X), _, Bevisat, A) :- 
    member([X, A, _], Bevisat).

% MT (A-->B, icke B |- icke A)
rule(mt(X, Y), _, Bevisat, neg(A)) :- 
    member([X, imp(A, B), _], Bevisat),
    member([Y, neg(B), _], Bevisat).

% PBC (Assume icke A.....⊥ |- A)
rule(pbc(X, Y), _, Bevisat, A) :- 
    box([X, neg(A), assumption], [Y, motsagelse, _], Bevisat).

% LEM
rule(lem, _, _, or(A, neg(A))).

% -->i (A (assumption),......,B |- A-->B)
rule(impint(X, Y), _, Bevisat, imp(A, B)) :- 
    box([X, A, assumption], [Y, B, _], Bevisat).

% -->e  (A, A-->B |- B)
rule(impel(X, Y), _, Bevisat, B) :- 
    member([X, A, _], Bevisat),
    member([Y, imp(A, B), _], Bevisat).

% ∨i 1
rule(orint1(X), _, Bevisat, or(A, _)) :- 
    member([X, A, _], Bevisat).

% ∨i 2
rule(orint2(X), _, Bevisat, or(_, A)) :- 
    member([X, A, _], Bevisat).

% ∨e assumption B leder till A och assumption C leder till A 
rule(orel(I, II, III, IV, V), _, Bevisat, A) :- 
    member([I, or(B, C), _], Bevisat),
    box([II, B, assumption], [III, A, _], Bevisat),
    box([IV, C, assumption], [V, A, _], Bevisat).
