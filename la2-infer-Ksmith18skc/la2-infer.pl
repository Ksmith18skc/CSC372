/*
Solutions for LA2 Assignment, Type inference for SML

Name: Kory Smith
Time spent on LA2: ~4-5 hrs

Collaborators and references: 
- Prolog documentation
- SML documentation
- SML type inference lecture notes
- Prolog lecture notes


*/

% --------------------------------------------------
% Entry point for type inference without needing to provide an environment
infer(Expr, Type) :- infer(Expr, Type, []).

% Used to supress a warning about infer/4 not being
% contiguous with the other infer/4 clauses.
:- discontiguous infer/3.
% --------------------------------------------------
% Basic types
infer(int(_), int, _).
infer(bool(_), bool, _).

% --------------------------------------------------
% Variable type lookup
infer(sml_var(X), Type, Env) :-
    lookup(X, Env, Type).


% --------------------------------------------------
% Generates a fresh Prolog variable for use as a type variable
fresh_type_var(_).

% --------------------------------------------------
% Lambda expression
% Type inference for lambda expressions
% (introducing a fresh environment scope)
infer(lambda(X, E), arrow(T1, T2), Env) :-
    fresh_type_var(T1),
    infer(E, T2, [(X, T1) | Env]).

% --------------------------------------------------
% Stubbed out applications so testing works.
% You need to implement these and potentially others.
% Application: apply(E1, E2)
infer(apply(E1, E2), T, Env) :-
    infer(E1, T1, Env),
    infer(E2, T2, Env),
    T1 = arrow(T2, T).
% --------------------------------------------------
% Let bindings: let(X, E1, E2)
infer(let(X, E1, E2), T, Env) :-
    infer(E1, T1, Env),
    infer(E2, T, [(X, T1) | Env]).
% --------------------------------------------------
% Conditional: if(E1, E2, E3)
infer(if(E1, E2, E3), T, Env) :-
    infer(E1, TCond, Env),
    TCond = bool,
    infer(E2, TThen, Env),
    infer(E3, TElse, Env),
    TThen = TElse,
    T = TThen.
% --------------------------------------------------
% Tuple: tuple(E1, E2)
infer(tuple(E1, E2), tuple(T1, T2), Env) :-
    infer(E1, T1, Env),
    infer(E2, T2, Env).
% --------------------------------------------------
% Environment lookup
lookup(Var, [(Var, Type) | _], Type).
lookup(Var, [_ | Rest], Type) :- lookup(Var, Rest, Type).

% --------------------------------------------------
% Catch-all clause for unhandled expressions
lookup(_,_,_) :- fail.

