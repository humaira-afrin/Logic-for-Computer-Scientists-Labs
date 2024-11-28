% For SICStus, uncomment line below: (needed for member/2)
%:- use_module(library(lists)).
% Load model, initial state and formula from file.
verify(Input) :-
see(Input), read(T), read(L), read(S), read(F), seen,
check(T, L, S, [], F).

% check(T, L, S, U, F)
% T - The transitions in form of adjacency lists
% L - The labeling
% S - Current state
% U - Currently recorded states
% F - CTL Formula to check.
%
% Should evaluate to true iff the sequent below is valid.
%
% (T,L), S |- F
% U
% To execute: consult('your_file.pl'). verify('input.txt').
% Literals

% kollar om X är en label [s,x] i current state S
%check(_, L, S, [], X) :- 
  member([S, Propositions], L), 
  member(X, Propositions).

% Neg X is false in D so neg(X) is true
%check(_, L, S, [], neg(X)) :-
  member([S, Propositions], L), 
  \+ member(X, Propositions). 

% And
%check(T, L, S, [], and(F,G)) :- 
  check(T, L, S, [], F),
  check(T, L, S, [], G). 
  
% Or
%check(T, L, S, [], or(F, G)) :-
  check(T, L, S, [], F);
  check(T, L, S, [], G). 

%Temporala operatorer
% AX
%check(T, L, S, [], ax(F)):- check(T, L, S, U, F),
member([S,Next],T),
next_all_path(T,L,Next,[],F).

% EX
%check(T, L, S, [], ex(F)):- check (T,L,S,[],F);
  member([S,Next], T),
  next_some_path(T,L,Next,[],F).

% AG1 
check(T,L,S,U,ag(F)):- member(S,U).
% AG 2
check(T,L,S,U,ag(F)):- \+member(S,U),
      check(T,L,S,[],F), member([S,Next],T),
      append([S],U,Recorded),
      next_all_path(T,L,Next,Recorded,ag(F)).

% EG1
check(T,L,S,U,eg(F)):- member(S,U).
%EG 2 Ngn path Next som har alla F
check(T,L,S,U,eg(F)):- \+member(S,U),
      check(T,L,S,[],F),
      member([S,Next],T),
      next_some_path(T,L,Next,[S|U],eg(F)).


% EF 1
check(T,L,S,U,ef(F)):- \member+(S,U),
      check(T,L,S,[],F).
% EF 1
check(T,L,S,U,ef(F)):-\member+(S,U), member([S,Next],T),
      append([S],U,Recorded).
      next_some_path(T,L,Next,Recorded,eg(F)).

% AF 1
check(T,L,S,U,af(F)):- \member+(S,U),
      check(T,L,S,[],F).
% AF 2
check(T,L,S,U,af(F)):-
% U is empty
next_all_path (T,L,[H|R],[],F):-check(T,L,H,[],F),
            check_states(T,L,R,[],F).
% U not empty
next_all_path(T,L,[H|R],U,F):-check(T,L,H,U,F),
            check_states(T,L,R,[H|U],F).

next_some_path (T,L,[H|R],[],F):-check(T,L,H,U,F);
        next_some_path(T,L,R,[],F).