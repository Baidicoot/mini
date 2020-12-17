```
 ______   @         @
|      |  _   ___   _
| /\/\ | | | | _ | | |
|/    \| |_| |/ \| |_|
```

Mini is a WIP functional (ML-style) programming language that has Scheme-like aspects.
Currently, it features:
- GADTs
- HM-style type inference
- compilation to C via CPS conversion
- a module system

And some planned features are:
- first-class modules
- record types
- modular implicits

### Syntax & Semantics
Mini syntax is fairly spartan, being a toy compiler; the only constructs it offers are `fix`, `let`, `ind` (datatype declaration), `match`, type annotation, `lam`, and function application.

#### Expressions
- `(fix (fn1 args def) (fn2 args def) expr)` defines the (recursive) functions `fn1` and `fn2` with arguments (like `(a b c)`) in `expr`.
- `(let (name1 def) (name2 def) expr)` evaluates both `def`s, then evaluates `expr` with `name1` and `name2` referring to the output of the relevant `def`. You can omit `name` in favour of `_` if `name` is unused in `expr`.
- `(match x (case1 -> branch1) (case2 -> branch2))` matches `x` with one of the cases of the `match`, and performs the corresponding `branch` - you can omit the outermost brackets on cases.
- `(lam (x y) expr)` denotes a lambda with (curried) arguments `x` and `y`, which evaluates `expr` with those arguments when called.
- `(a :: t)` declares to the compiler that the (bracketed) expression `a` is of type `t`.

#### Top-Level Expressions
- `(ind Name (case1 :: type1) (case2 :: type2))` declares a GADT with (capitalised) name `Name` which has constructors `case1` of type `type1`, and `case2` of type `type2`.
- `(fix (fn1 args def) (fn2 args def))` is like the expression-level `fix`, except the resultant expression is ommitted.
- `(let (name1 def) (name2 def))` is like the expression-level `let`, except the resultant expression is ommitted.
- `(fn1 args def)` is shorthand for `(fix (fn1 args def))`

#### Annotations on Definitions
- `(fix ((f :: a -> b -> a) (a b) a) exp)` annotates the created lambdas when `f` is elaborated.
- `(let ((a :: b) b) exp)` is equivalent to `(let (a (b :: b)) exp)`.

### Examples
Here is the factorial function in Mini:
```lisp
(fac x match (eq x 0)
    (true -> 1)
    (false -> * x (fac (- x 1))))

(let
    (_ putint (fac 6)))
```
i.e. `fac` is a function that takes one argument, `x`, and checks if `x` equals `0`:
- if it does, `fac` returns `1`
- if it doesn't, `fac` returns x times the factorial of `x`'s predecessor

The program then calculates `fac 6`, and outputs it.

This demonstrates some of Mini's key features;
- tail-call optimisation (a Mini program will never\* stack overflow)
- pattern matching
- top-level expressions are executed when the module is called

The `(let (_ putint (fac 6)))` top-level expression shows how anonymized - hence the `_` - let-expressions can be used to perform side-effecting operations without having to use sequencing operators like `(seq (a b) b)`.