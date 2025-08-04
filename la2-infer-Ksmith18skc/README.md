[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/yetttbmD)
# la2-typeinfer

Answer the below questions and submit this edited file to Gradescope along with la2-infer.pl.

### fresh prolog variables [2 pts]

Do a trace on some of the lambda test cases in la2-test.pl.  How does the `fresh_type_var` predicate work?

 `fresh_type_var(T)` generates a fresh, unbound variable that serves as a type variable placeholder. It has not been unified with any value.

### why no polymorphism? [2 pts]

Do a trace of the following query:
```
infer(let(f,lambda(x, sml_var(x)),tuple(apply(sml_var(f),int(3)),apply(sml_var(f),bool))),T).
```
Why does it fail?

It fails because the system is monomorphic and does not support polymorphism.



### Explain a trace [4 points]

Trace `infer(lambda(f, lambda(x, apply(sml_var(f), apply(sml_var(f), sml_var(x))))),T).`.
At which point in the trace does the type of `f` go from being `T1 -> T2` 
When the second apply() is excecuted

to being `T -> T`?

Same, when second apply is processed

Explain why that happens.

This happens because at that point, Prolog unifies f's input and output types because f is applied to its own result, forcing T1 = T2.