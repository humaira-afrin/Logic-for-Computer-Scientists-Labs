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
%check(_, L, S, [], X) :- 
  member([S, Propositions], L), 
  member(X, Propositions).
%Neg
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
%check(T, L, S, [], ax(F)):- 
% EX
%check(T, L, S, [], ex(F)):- 
% AG
% EG
% EF
% AF
