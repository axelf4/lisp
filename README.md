# lisp

Bytecode interpreter for a small imperative Lisp-1 dialect.

## Language reference

Forms are evaluated according to the following rules:

* `(F ...)` where `F` is a special form is evaluated as per below.
* `(M ARG...)` where `M` is a symbol *globally* bound to `(macro . F)`
  gets recursively expanded to the result of applying `F` to `ARG...`,
  *before* the top-level form is evaluated.
* `(F ARG...)` first evaluates `F` and then applies the result to
  the result of evaluating each of `ARG...`.

  Tail-call elimination in return-positions is guaranteed.
* `SYM` evaluates to the value of lexically-scoped variable named `SYM`.
* `nil` and integers evaluate to themselves.

The language consists of the following special forms:

* `(fn (ARG...) BODY...)`

  Returns a new closure.
* `(progn BODY...)`

  Evaluates the `BODY` forms in sequence and returns the value of the last one.
  If `BODY` is empty, the value is nil.
* `(let ([(SYM VAL)]...) BODY...)`

  Evaluates `(progn BODY...)`
  with each `SYM` bound to the value of its corresponding `VAL` form,
  which may refer to earlier `SYM`s.
* `(set SYM VAL)`

  Sets the value of the lexically-scoped variable named `SYM`
  to the result of evaluating `VAL`.
* `(if COND THEN ELSE...)`

  If `COND` evaluates to a non-nil value, then `THEN` is evaluated,
  otherwise `(progn ELSE...)` is evaluated.
* `(quote ARG)`

  Returns `ARG` unevaluated.
  The reader shorthand `'` exists, e.g. `'a` is the same as `(quote a)`.

The reader perpetuates the mistake of `quote` returning reader constants.

Furthermore, currently the following set of built-in functions are defined:
`print`, `cons`, `car`, `cdr`, `+` and `<`.

## Background

This project is both an opportunity to learn about writing Lisp interpreters,
and an exploration of the Emacs-esque Lisp environment design space.
While I prefer immutable statically-typed languages in general,
interactive programming, which is inherently mutable, has its upsides.
The uniform syntax of Lisp is good at facilitating selective evaluation of subexpressions.

GNU Emacs, while a decent program, has a couple of aspects
that I would like to see if they can be done differently:

* GNU Emacs 28 introduced an AOT Lisp compiler to native code.
  However, given the dynamic nature of the Lisp environment
  there are very few "safe" optimizations it can perform.
  Indeed, it still does performs some naive optimizations
  such as inlining redefinable built-in functions.
  As such, the speedup mostly comes from making function calls cheaper.
  More interesting would be a JIT compiler,
  that was able to specialize on the observed types of runtime values.
* "Symbols with position" were introduced to be able to include the source code span
  of the offending construct in error reports.
  This meant that all places in the code that interacted with symbols
  now has to have a branch on whether the symbol has a position attached...
  Instead, the standard solution used by all other interpreters is to just
  have the reader return Token × Span pairs
  (possibly interleaving the reader and the compiler to make it zero-cost),
  and storing a compressed map from bytecode to line numbers in Lisp function values.
* GNU Emacs uses gap buffers to store buffer text which is a fine choice.
  However, that means line length caches must be maintained separately.
  If a rope was used instead it could easily be adapted to carry also that information.
* Symbol properties should not be.

  Instead "open closures," i.e. closures with plists, should be embraced,
  as they already have uses for
  documentation strings, `interactive` specifications and advices.
* Designing the interpreter from the start not to rely on global state,
  makes it easy to spin up unsafe parallel interpreter threads.
  It is terrible compared to the state-of-the-art in parallelism
  offered by purely functional languages,
  but it is also a very Lispy solution.
* To avoid duplication, this project will not implement a tree-walking interpreter,
  in addition to the bytecode interpreter.

  This means the bytecode compiler has to be written in C instead of Lisp.
  On the other hand, it will actually be fast.
