:- begin_tests(tests).
:- consult('la2-infer.pl').

% Test for integer literals
test(inferInt1) :- 
    once(infer(int(5), int)).
test(inferInt2) :- 
    once(infer(int(-100), int)).
test(inferInt3) :- 
    not(once(infer(int(0), bool))).


% Test for boolean literals
test(inferBool1) :- 
    once(infer(bool(true), bool)).
test(inferBool2) :- 
    once(infer(bool(false), bool)).

% Test for variable type lookup
test(inferVar1) :-
    once(infer(sml_var(x), int, [(x, int)])).
test(inferVar2) :-
    once(infer(sml_var(y), bool, [(y, bool)])).
test(inferVar3) :-
    not(once(infer(sml_var(z), int, [(z, bool)]))).

% Test for tuples
test(inferTuple1) :-
    once(infer(tuple(int(1), bool(true)), tuple(int, bool))).
test(inferTuple2) :-
    once(infer(tuple(bool(false), int(2)), tuple(bool, int))).
test(inferTuple3) :-
    not(once(infer(tuple(int(1), int(2)), tuple(bool, int)))).

% Test for if expressions
test(inferIf1) :-
    once(infer(if(bool(true), int(1), int(0)), int)).
test(inferIf2) :-
    once(infer(if(bool(false), bool(true), bool(false)), bool)).
test(inferIf3) :-
    not(once(infer(if(int(1), bool(true), bool(false)), _))).
test(inferIf4) :-
    not(once(infer(if(bool(true), int(1), bool(false)), _))).

% Test for let expressions
test(inferLet1) :- 
    once(infer(let(x, int(1), sml_var(x)), int)).
test(inferLet2) :- 
    once(infer(let(x, bool(true), sml_var(x)), bool)).
test(inferLet3) :- 
    not(once(infer(let(x, int(1), sml_var(x)), bool))).
test(inferLet4) :- 
    not(once(infer(let(x, bool(true), sml_var(x)), int))).

% Test for lambda expressions
test(inferLambda1) :- 
    once((infer(lambda(x, int(1)), arrow(T, int)),var(T))).
test(inferLambda2) :- 
    once((infer(lambda(x, sml_var(x)), arrow(T, T)), var(T))).
test(inferLambda3) :- 
    once((infer(lambda(x, bool(true)), arrow(T, bool)),var(T))).

% Test for function application
test(inferApply1) :- 
    once(infer(apply(lambda(x, sml_var(x)), int(1)), int)).
test(inferApply2) :- 
    once(infer(apply(lambda(x, sml_var(x)), bool(true)), bool)).
test(inferApply3) :- 
    not(once(infer(apply(lambda(x, sml_var(x)), int(1)), bool))).
test(inferApply4) :- 
    not(once(infer(apply(lambda(x, sml_var(x)), bool(true)), int))).

% Test for applying a function twice
test(applyTwice) :-
    once((infer(lambda(f, lambda(x, 
        apply(sml_var(f), apply(sml_var(f), sml_var(x))))),
        arrow(arrow(T,T),arrow(T,T))),var(T))).

% Test for two type variables
test(twoTypeVars1) :-
    once((infer(lambda(y, lambda(x, 
        apply(sml_var(y), sml_var(x)))),
        arrow(arrow(T,T),arrow(T,T))),var(T))).
test(twoTypeVars2) :-
    once((infer(lambda(y, lambda(x, 
        tuple(sml_var(y), sml_var(x)))),
        arrow(T1,arrow(T2,tuple(T2,T1)))),var(T1),var(T2))).

% Test for a let that binds a lambda to a variable
% and then uses it in an application.
test(letLambdaApply) :-
    once(infer(let(f, lambda(x, sml_var(x)), 
        apply(sml_var(f), int(1))),
        int)).

:- end_tests(tests).
:- set_prolog_flag(verbose, true).
:- run_tests.
:- halt.
